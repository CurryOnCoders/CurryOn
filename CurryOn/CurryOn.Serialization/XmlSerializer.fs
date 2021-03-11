namespace CurryOn.Serialization

open CurryOn
open System
open System.IO
open System.Net.Http

type XmlSerializer () =
    let getSerializer (objectType: Type) = 
        System.Xml.Serialization.XmlSerializer(objectType)

    let serializeToStream (stream: Stream) value =
        result {
            let serializer = value.GetType() |> getSerializer
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
            value |> serializeToString |> Result.map (fun s -> new StringContent(s, Utf8.encoding, "application/xml") :> HttpContent)

        member __.Deserialize<'t> bytes =
            result {
                let serializer = typeof<'t> |> getSerializer
                use stream = new MemoryStream(bytes)
                use reader = new StreamReader(stream)
                try                    
                    return serializer.Deserialize(reader) |> unbox<'t>
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingBytes (bytes, objectType, ex)
            }

        member __.DeserializeString<'t> str =
            result {
                let serializer = typeof<'t> |> getSerializer
                use reader = new StringReader(str)
                try                    
                    return serializer.Deserialize(reader) |> unbox<'t>
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingString (str, objectType, ex)
            }

        member __.DeserializeStream<'t> stream =
            result {
                let serializer = typeof<'t> |> getSerializer
                try                    
                    return serializer.Deserialize(stream) |> unbox<'t>
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }

        member __.DeserializeContent<'t> content =
            asyncResult {
                let serializer = typeof<'t> |> getSerializer
                use! stream = content.ReadAsStreamAsync().ToAsyncResult(UnexpectedSerializationError)
                try                    
                    return serializer.Deserialize(stream) |> unbox<'t>
                with ex ->
                    let objectType = typeof<'t>
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }

        member __.DeserializeAsType objectType bytes =
            result {
                let serializer = getSerializer objectType
                use stream = new MemoryStream(bytes)
                use reader = new StreamReader(stream)
                try                    
                    return serializer.Deserialize(reader)
                with ex ->
                    return! Error <| ErrorDeserializingBytes (bytes, objectType, ex)
            }

        member __.DeserializeStringAsType objectType str =
            result {
                let serializer = getSerializer objectType
                use reader = new StringReader(str)
                try                    
                    return serializer.Deserialize(reader)
                with ex ->
                    return! Error <| ErrorDeserializingString (str, objectType, ex)
            }

        member __.DeserializeStreamAsType objectType stream =
            result {
                let serializer = getSerializer objectType
                try                    
                    return serializer.Deserialize(stream)
                with ex ->
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }

        member __.DeserializeContentAsType objectType content =
            asyncResult {
                let serializer = getSerializer objectType
                use! stream = content.ReadAsStreamAsync().ToAsyncResult(UnexpectedSerializationError)
                try                    
                    return serializer.Deserialize(stream)
                with ex ->
                    return! Error <| ErrorDeserializingStream (objectType, ex)
            }
