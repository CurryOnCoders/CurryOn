namespace CurryOn.Serialization

open CurryOn
open FSharp.Reflection

open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization

open System
open System.Collections.Concurrent
open System.Globalization
open System.IO
open System.Net.Http
open System.Reflection

/// Enable transparent serialization of F# Discriminated Unions
type TransparentUnionAttribute() =
    inherit Attribute()

/// Alias for TransparentUnion to be consistent with OpenAPI-style usage
type OneOfAttribute = TransparentUnionAttribute

/// Enable OpenAPI 'AllOf' style serialization behavior for an object
type AllOfAttribute () =
    inherit Attribute()


[<AbstractClass>]
type AttributeBasedConverter() as converter =
    inherit JsonConverter()
    let hasAttribute = memoize <| fun (objectType: Type) ->
        objectType.GetCustomAttributes(true) |> Seq.tryFind (fun attribute -> attribute.GetType() = converter.Attribute) |> Option.isSome
    abstract member Attribute: Type        
    override this.CanConvert (objectType: Type) =
        hasAttribute objectType

[<AbstractClass>]
type AttributeBasedConverter<'attribute when 'attribute :> Attribute> () =
    inherit AttributeBasedConverter()
    override __.Attribute = typeof<'attribute>

type FlexibleReadUniformWriteIsoDateTimeConverter () =
    inherit IsoDateTimeConverter(DateTimeStyles = DateTimeStyles.AdjustToUniversal, DateTimeFormat = "yyyy'-'MM'-'dd'T'HH':'mm':'ss.fffK")

    override __.ReadJson (reader, clrType, initialValue, serializer) =
        match box reader.Value with
        | :? DateTime as date -> 
            date.ToUniversalTime() |> box
        | other ->
            let dateString = other.ToString()
            match dateString |> tryParse<DateTime> with
            | Some date -> date.ToUniversalTime() |> box
            | None -> raise <| JsonSerializationException(sprintf "The value %s could not be deserialized as a Date/Time" dateString)

type OneOfConverter() =
    inherit AttributeBasedConverter<TransparentUnionAttribute>()
    let defaultSerializer = JsonSerializer.CreateDefault()

    static let tryFindKnownType, addKnownType = 
        let getKey (clrType: Type) (json: JObject) =
            match json.Properties() |> Seq.map (fun p -> p.Name) |> Seq.toList with
            | [] -> json.ToString()
            | head :: tail -> tail |> List.fold (sprintf "%s_%s") head
            |> (sprintf "%s-%s" clrType.FullName)

        let knownTypes = ConcurrentDictionary<string, UnionCaseInfo * Type>()
        
        (fun clrType json ->
            let key = getKey clrType json
            match knownTypes.TryGetValue(key) with
            | (true, value) -> Some value
            | _ -> None),

        (fun clrType json (unionCase, dataType) ->
            let key = getKey clrType json
            knownTypes.AddOrUpdate(key, (unionCase, dataType), fun _ _ -> (unionCase, dataType)) |> ignore)

    let deserializeUnion unionCase dataType (json: JObject) serializer =
        let deserializedObject = json.ToObject(dataType, serializer)
        FSharpValue.MakeUnion(unionCase, [| deserializedObject |], true)

    let tryMatchCase (json: JObject) serializer (unionCase: UnionCaseInfo) =
        unionCase.GetFields() 
        |> Seq.tryHead 
        |> Option.map (fun data -> data.PropertyType)
        |> Option.bind (fun dataType ->
            if dataType.GetProperties() |> Seq.forall (fun field -> json.ContainsKey(camelCase field.Name)) then
                try
                    let populatedUnion = deserializeUnion unionCase dataType json serializer
                    (populatedUnion, unionCase, dataType) |> Some
                with _ ->
                    None
            else 
                None)

    let findCaseByValue objectType (unionCases: UnionCaseInfo []) (reader: JsonReader) = 
        let unionCase = unionCases |> Array.tryFind (fun case -> 
            let fields = case.GetFields()
            fields.Length = 1 && fields.[0].PropertyType = reader.ValueType)
        match unionCase with
        | Some case -> FSharpValue.MakeUnion(case, [|reader.Value|], true)
        | None -> defaultSerializer.Deserialize(reader, objectType) 

    override __.WriteJson (writer, value, serializer) =
        let (unionCaseInfo, unionFields) = FSharpValue.GetUnionFields(value, value.GetType(), true)
        let serialValue = 
            match unionFields with
            | [||] -> unionCaseInfo.Name |> box
            | [|singleValue|] -> singleValue |> box
            | multipleValues -> multipleValues |> box
        serializer.Serialize(writer, serialValue)

    override __.ReadJson (reader, objectType, initialValue, serializer) =
        let unionCases = getUnionCases objectType 
        match reader.TokenType with
        | JsonToken.StartObject ->                
            let json = JObject.Load(reader)
            match tryFindKnownType objectType json with
            | Some (unionCase, dataType) ->
                deserializeUnion unionCase dataType json serializer
            | None ->
                match unionCases |> Array.tryPick (tryMatchCase json serializer) with
                | Some (value, case, dataType) -> 
                    (case, dataType) |> addKnownType objectType json
                    value
                | None -> 
                    defaultSerializer.Deserialize(reader, objectType)                
        | JsonToken.String ->
            let unionCase = unionCases |> Array.tryFind (fun case -> case.Name |> String.like (reader.Value |> unbox<string>))
            match unionCase with
            | Some case -> 
                FSharpValue.MakeUnion(case, [||], true)
            | None -> 
                reader |> findCaseByValue objectType unionCases   
        | _ -> 
            reader |> findCaseByValue objectType unionCases   

type private Property =
    | Normal of PropertyInfo
    | AllOf of AllOfProperty
    member this.Name =
        match this with
        | Normal p -> p.Name
        | AllOf p -> p.Property.Name
    member this.Type =
        match this with
        | Normal p -> p.PropertyType
        | AllOf p -> p.Property.PropertyType

and private AllOfProperty =
    {
        Property: PropertyInfo
        Instance: obj
        Fields: Property list
    }
    member this.HasField name = 
        this.Fields |> Seq.tryPick (fun field -> 
            match field with
            | Normal p -> 
                if p.Name |> String.like name 
                then Some p.PropertyType
                else None
            | AllOf p ->
                p.HasField(name))                
    member this.SetField(property, value) =
        for field in this.Fields do
            match field with
            | Normal p -> 
                if p.Name |> String.like property 
                then p.SetValue(this.Instance, value)
            | AllOf p ->
                p.SetField(property, value)

type AllOfConverter () =
    inherit AttributeBasedConverter<AllOfAttribute>()

    let isObject (clrType: Type) =
        match Type.GetTypeCode(clrType) with
        | TypeCode.Object -> true
        | _ -> false

    let rec getProperty (container: Type) (parent: Type) (property: PropertyInfo) =
        if property.PropertyType.IsArray || property.PropertyType |> isObject |> not || parent.GetCustomAttribute<AllOfAttribute>() |> isNull then
            Normal property
        else
            let instance = Activator.CreateInstance(property.PropertyType)
            let fields = getProperties parent property.PropertyType
            AllOf {Property = property; Instance = instance; Fields = fields}                

    and getProperties (container: Type) (clrType: Type) =
        clrType.GetProperties() |> Seq.map (getProperty container clrType) |> Seq.toList

    let rec getPopulatedProperties (clrType: Type) (parent: obj) =
        if clrType.GetCustomAttribute<AllOfAttribute>() |> isNull then
            clrType.GetProperties() |> Array.map (fun p -> p.Name, p.GetValue(parent))
        else
            clrType.GetProperties() |> Array.collect (fun p -> p.GetValue(parent) |> getPopulatedProperties p.PropertyType)

    let rec setInstanceValues parentObject (properties: AllOfProperty list) =
        for property in properties do
            property.Fields 
            |> List.choose (fun field ->
                match field with
                | Normal _ -> None
                | AllOf p -> Some p)
            |> setInstanceValues property.Instance
            property.Property.SetValue(parentObject, property.Instance)

    override __.WriteJson (writer, value, serializer) =
        let valueType = value.GetType()
        let properties = value |> getPopulatedProperties valueType
        writer.WriteStartObject()
        for (name, fieldValue) in properties do
            let fieldName = camelCase name
            writer.WritePropertyName(fieldName)
            serializer.Serialize(writer, fieldValue)
        writer.WriteEndObject()

    override __.ReadJson (reader, objectType, initialValue, serializer) =
        let realObjectType =
            if objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<Nullable<_>> then
                objectType.GetGenericArguments() |> Seq.tryHead |> Option.defaultValue objectType
            else
                objectType

        let objectMap =
            realObjectType.GetProperties() 
            |> Seq.map (fun property -> 
                let fields = getProperties realObjectType property.PropertyType
                let instance = Activator.CreateInstance(property.PropertyType)
                {Property = property; Instance = instance; Fields = fields})
            |> Seq.toList
        let json = JObject.Load(reader)
        for jsonProperty in json.Properties() do 
            for property in objectMap do
                match property.HasField(jsonProperty.Name) with
                | Some fieldType ->
                    let fieldValue = jsonProperty.Value.ToObject(fieldType, serializer)
                    property.SetField(jsonProperty.Name, fieldValue)
                | None ->
                    ()
        let allOfObject = Activator.CreateInstance(realObjectType)
        objectMap |> setInstanceValues allOfObject
        allOfObject


module internal JsonSettings =

    let Default = JsonSerializerSettings(
                    ContractResolver = CamelCasePropertyNamesContractResolver(),
                    DateFormatHandling = DateFormatHandling.IsoDateFormat,
                    NullValueHandling = NullValueHandling.Include,
                    DefaultValueHandling = DefaultValueHandling.Include,
                    MissingMemberHandling = MissingMemberHandling.Ignore,
                    TypeNameHandling = TypeNameHandling.None,
                    Converters = [| StringEnumConverter(); |])

    let CurryOn = JsonSerializerSettings(
                        ContractResolver = Default.ContractResolver,
                        DateFormatHandling = Default.DateFormatHandling,
                        NullValueHandling = Default.NullValueHandling,
                        DefaultValueHandling = Default.DefaultValueHandling,
                        MissingMemberHandling = Default.MissingMemberHandling,
                        TypeNameHandling = Default.TypeNameHandling,
                        Converters = [| StringEnumConverter(); FlexibleReadUniformWriteIsoDateTimeConverter(); OneOfConverter(); AllOfConverter(); |]
                    )

type JsonSerializer () =
    let serializer = Newtonsoft.Json.JsonSerializer.Create(JsonSettings.CurryOn)

    let serializeToStream (stream: Stream) value =
        result {
            use writer = new StreamWriter(stream)            
            try
                serializer.Serialize(writer, value)
            with ex ->
                let valueType = value.GetType()
                return! Error <| ErrorSerializingType (valueType, ex)
        }

    let serialize value =
        result {
            use stream = new MemoryStream()            
            do! serializeToStream stream value
            return stream.ToArray()
        }

    let serializeToString value =
        value |> serialize |> Result.map Utf8.toString

    interface IJsonSerializer with
        member __.Serialize<'t> (value: 't) =
            serialize value
        member __.SerializeToString<'t> (value: 't) =
            serializeToString value
        member __.SerializeToStream<'t> stream (value: 't) =
            value |> serializeToStream stream
        member __.SerializeToContent<'t> (value: 't) =
            value |> serializeToString |> Result.map (fun s -> new StringContent(s, Utf8.encoding, "application/json") :> HttpContent)

        member __.Deserialize<'t> bytes =
            result {
                use stream = new MemoryStream(bytes)
                use reader = new StreamReader(stream)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize<'t>(json)
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingBytes (bytes, objectType, ex)
            }

        member __.DeserializeString<'t> str =
            result {
                use reader = new StringReader(str)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize<'t>(json)
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingString (str, objectType, ex)
            }

        member __.DeserializeStream<'t> stream =
            result {
                use reader = new StreamReader(stream)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize<'t>(json)
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }

        member __.DeserializeContent<'t> content =
            asyncResult {
                use! stream = content.ReadAsStreamAsync().ToAsyncResult(UnexpectedSerializationError)
                use reader = new StreamReader(stream)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize<'t>(json)
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }

        member __.DeserializeAsType objectType bytes =
            result {
                use stream = new MemoryStream(bytes)
                use reader = new StreamReader(stream)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize(json, objectType)
                with ex ->
                    return! Error <| ErrorDeserializingBytes (bytes, objectType, ex)
            }

        member __.DeserializeStringAsType objectType str =
            result {
                use reader = new StringReader(str)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize(json, objectType)
                with ex ->
                    return! Error <| ErrorDeserializingString (str, objectType, ex)
            }

        member __.DeserializeStreamAsType objectType stream =
            result {
                use reader = new StreamReader(stream)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize(json, objectType)
                with ex ->
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }

        member __.DeserializeContentAsType objectType content =
            asyncResult {
                use! stream = content.ReadAsStreamAsync().ToAsyncResult(UnexpectedSerializationError)
                use reader = new StreamReader(stream)
                use json = new Newtonsoft.Json.JsonTextReader(reader)
                try                    
                    return serializer.Deserialize(json, objectType)
                with ex ->
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }