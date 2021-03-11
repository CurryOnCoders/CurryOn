namespace CurryOn.DependencyInjection

open CurryOn
open System
open System.IO
open System.Reflection
open System.Runtime

[<AutoOpen>]
module Extensions =
    let internal exists : obj -> bool = box >> isNull >> not

    type Type with
        member this.Implements (clrType: Type) =
            clrType.IsAssignableFrom(this)
        member this.Implements<'t> () =
            this.Implements(typeof<'t>)        
        member this.IsInjectable = 
            not (this.IsAbstract || this.IsInterface)
        member this.IsInjectableAs (clrType: Type) =
            this.IsInjectable && this.Implements(clrType)
        member this.IsInjectableAs<'t> () =
            this.IsInjectableAs typeof<'t>        
        member this.IsConstructable() =
            this.GetConstructors() |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0)

    type ICustomAttributeProvider with
        member this.GetAttributes<'a when 'a :> Attribute>(?inherits: bool) =
            this.GetCustomAttributes(typeof<'a>, inherits |> Option.defaultValue false)
            |> Array.map unbox<'a>

module Types =
    let private tryLoadTypes (assembly: Assembly) =
        try 
            assembly.GetTypes() 
        with _ ->
            [||]

    let private getTypes : Assembly seq -> Type [] =
        Seq.collect tryLoadTypes
        >> Seq.distinctBy (fun t -> t.FullName)        
        >> Seq.toArray

    let private tryLoadAssembly (dll: FileInfo) =
        try 
            Some <| Assembly.LoadFrom(dll.FullName) 
        with _ -> 
            None 

    let private getDirectoryTypes = memoize <| fun path ->
        let baseDirectory = DirectoryInfo path
        baseDirectory.EnumerateFiles("*.dll", SearchOption.TopDirectoryOnly)   
        |> Seq.choose tryLoadAssembly
        |> getTypes

    let private appDomainTypes = lazy(
        AppDomain.CurrentDomain.GetAssemblies() |> getTypes)

    let private searchPaths =
        let appDomain = AppDomain.CurrentDomain
        [
            Environment.CurrentDirectory
            appDomain.BaseDirectory
            appDomain.DynamicDirectory
            Path.Combine(appDomain.BaseDirectory, appDomain.RelativeSearchPath)
        ] |> List.distinct

    let internal types =
        seq {
            for t in appDomainTypes.Value -> t
            for path in searchPaths do
                for t in getDirectoryTypes path -> t
        } |> Seq.distinctBy (fun t -> t.FullName)

    let collect (f: Type -> 'a) =
        types |> Seq.collect f

    let filter (f: Type -> bool) =
        types |> Seq.filter f

    let find (f: Type -> bool) =
        types |> Seq.find f

    let tryFind (f: Type -> bool) =
        types |> Seq.tryFind f

    let tryGet name = 
        let exactType = Type.GetType name
        if exactType |> isNull
        then None
        else Some exactType    

    let tryFindByName (name: string) =
        match tryGet name with
        | None ->
            let searchName =
                if name.EndsWith("[]")
                then name.Substring(0, name.Length - 2)
                else name
            let foundType =
                tryFind (fun clrType -> 
                    if searchName.Contains(".")
                    then let typeNamespace = searchName.Substring(0, searchName.LastIndexOf('.'))
                         let typeName = searchName.Substring(searchName.LastIndexOf('.') + 1)
                         clrType.Namespace = typeNamespace && clrType.Name = typeName
                    else clrType.Name = name)
            match foundType with
            | Some clrType -> 
                if searchName = name
                then clrType |> Some
                else clrType.MakeArrayType() |> Some
            | None -> None
        | found -> found

    let tryFilter f = Seq.filter (fun e -> try f e with _ -> false)

    let decorated<'a when 'a :> Attribute> () =
        types |> tryFilter (fun t -> t.GetCustomAttribute<'a>(true) |> exists)

    let decoratedBy (attribute: Type) =
        types |> tryFilter (fun t -> t.GetCustomAttribute(attribute, true) |> exists)

module Attribute =
    let tryGet<'attribute when 'attribute :> Attribute> (clrType: Type) =
        clrType.GetAttributes<'attribute>(true) |> Seq.tryHead

module Attributes =
    let private methods = lazy(Types.collect (fun t -> t.GetMethods()) |> Seq.toList)
    let private properties = lazy(Types.collect (fun t -> t.GetProperties()) |> Seq.toList)
    let private safeCheck f v = if v |> isNull then false else f v

    let hasAttribute<'a when 'a :> Attribute> (element: ICustomAttributeProvider) =
        element.GetAttributes<'a>().Length > 0

    let inline private tryGet<'attribute, ^decorated when 'attribute :> Attribute and ^decorated : (member GetCustomAttributes : bool -> obj [])> (decorated: ^decorated) =
       (^decorated : (member GetCustomAttributes : bool -> obj []) (decorated, true))
       |> Seq.tryFind (fun attribute -> attribute.GetType() = typeof<'attribute>)
       |> Option.map unbox<'attribute>
       |> Option.map (fun attribute -> decorated, attribute)

    let inline private tryGetWith< ^decorated when ^decorated : (member GetCustomAttributes : Type -> bool -> obj [])> (attributeType: Type) (decorated: ^decorated) =
       (^decorated : (member GetCustomAttributes : Type -> bool -> obj []) (decorated, attributeType, true))
       |> Seq.tryFind (fun attribute -> attribute.GetType() = attributeType)
       |> Option.map unbox<Attribute>
       |> Option.map (fun attribute -> decorated, attribute)

    let findTypes<'attribute when 'attribute :> Attribute> () =
        Types.decorated<'attribute>()
        |> Seq.map (fun t -> t, t.GetCustomAttribute<'attribute>(true))

    let findTypesWith (attribute: Type) =
        attribute |> Types.decoratedBy |> Seq.map (fun t -> t, t.GetCustomAttribute(attribute, true))

    let findMethods<'attribute when 'attribute :> Attribute> () =
        methods.Value |> Seq.choose tryGet<'attribute, MethodInfo>

    let findMethodsWith (attribute: Type) =
        methods.Value |> Seq.choose (tryGetWith attribute)

    let findProperties<'attribute when 'attribute :> Attribute> () =
        properties.Value |> Seq.choose tryGet<'attribute, PropertyInfo>

    let findPropertiesWith (attribute: Type) =
        properties.Value |> Seq.choose (tryGetWith attribute)

    let findFunctions<'attribute, 'a, 'b when 'attribute :> Attribute> () =
        findProperties<'attribute>()
        |> Seq.filter (fun (property, _) -> property.GetGetMethod() |> safeCheck (fun p -> p.IsStatic))
        |> Seq.filter (fun (property, _) -> property.PropertyType = typeof<'a -> 'b>)
        |> Seq.map (fun (property, attribute) -> property.GetValue(null) |> unbox<'a -> 'b>, attribute)
        |> Seq.append (
            findMethods<'attribute>()
            |> Seq.filter (fun (m, _) -> m.IsStatic)
            |> Seq.filter (fun (m, _) -> 
                match m.GetParameters() with
                | parameters when parameters.Length = 1 -> parameters.[0].ParameterType = typeof<'a> && m.ReturnType = typeof<'b>
                | _ -> false)
            |> Seq.map (fun (m, attribute) -> (fun (p: 'a) -> m.Invoke(null, [|p|]) |> unbox<'b>), attribute))
