namespace CurryOn.Mapping

open AutoMapper
open CurryOn
open System
open System.Reflection
open System.Linq.Expressions

module Mapper =
    type private MapperType = class end

    let private isProfile t = typeof<Profile>.IsAssignableFrom(t)

    let private createProfile t =
        try Activator.CreateInstance(t) |> unbox<Profile> |> Some
        with _ -> None

    let getProfiles () =
        let types = 
            AppDomain.CurrentDomain.GetAssemblies()
            |> Array.collect(fun asm ->
                try asm.GetTypes()
                with _ -> [||]
            )
        types
        |> Seq.collect (fun t -> t.GetProperties(BindingFlags.Public ||| BindingFlags.Static))
        |> Seq.filter (fun p -> p.PropertyType |> isProfile)
        |> Seq.map (fun p -> p.GetValue(null))
        |> Seq.cast<Profile>
        |> Seq.append (types |> Seq.filter isProfile |> Seq.choose createProfile)

    let private getMapper =
        let mapper = 
            lazy(
                try
                    let mapperConfigurationExpression = Configuration.MapperConfigurationExpression()
                    mapperConfigurationExpression.AddMaps([typeof<MapperType>.Assembly])           
                    mapperConfigurationExpression.AddProfiles(getProfiles ())
                    let mapperConfiguration = new MapperConfiguration(mapperConfigurationExpression)
                    mapperConfiguration.AssertConfigurationIsValid()
                    Ok <| mapperConfiguration.CreateMapper()
                with  ex ->
                    Error <| AutoMapperInitializationFailed ex
            )
        fun () -> !mapper

    /// Run a map using the static profiles discovered at initialization
    let map<'source, 'destination> (source: 'source) = 
        try 
            getMapper()
            |> Result.map (fun mapper -> mapper.Map<'source, 'destination>(source))
        with ex -> 
            Error <| ErrorMappingTypes (typeof<'source>, typeof<'destination>, ex)
         
    let private isFsharpCollection (clrType: Type) =
        match clrType.GetGenericTypeDefinition() with
        | list when list = typedefof<List<_>> -> true
        | set when set = typedefof<Set<_>> -> true
        | map when map = typedefof<Map<_,_>> -> true
        | _ -> false

    /// Create a one-time-use mapper with the given profile and map the input object using that mapper 
    let mapOnce<'source, 'destination> (profile: Profile) (source: 'source) =
        try
            let destinationType = typeof<'destination>
            let propertyTypes = destinationType.GetProperties() |> Array.map (fun property -> property.PropertyType)
            for propertyType in propertyTypes |> Array.filter (fun property -> property.IsGenericType && property |> isFsharpCollection) do    
                let mutable isMapped = false
                profile.ForAllMaps(fun map _ -> if map.DestinationType = propertyType then isMapped <- true)
                if isMapped |> not then
                    let signature = [| propertyType; propertyType; |]
                    let method = profile.GetType().GetMethod("CreateMap", [||]).MakeGenericMethod(signature)
                    let mapping = method.Invoke(profile, [||])                     
                    let converterType = typedefof<Func<_,_>>.MakeGenericType(signature)
                    let convertUsing = mapping.GetType().GetMethod("ConvertUsing", [| converterType |])
                    let p1 = Expression.Parameter(propertyType)
                    let f = Expression.Lambda(p1, [p1]).Compile()
                    convertUsing.Invoke(mapping, [| f |]) |> ignore

            let config = MapperConfiguration(fun cfg -> cfg.AddProfile(profile))
            let mapper = config.CreateMapper()
            Ok <| mapper.Map<'source, 'destination>(source)
        with ex ->
            Error <| ErrorMappingTypes (typeof<'source>, typeof<'destination>, ex)

    let getGenericMappingFunction (sourceType: Type) (destinationType: Type) =
        try
            typeof<MapperType>.DeclaringType.GetMethods()
            |> Seq.tryFind (fun m -> m.Name = "map")
            |> Option.map (fun m -> m.MakeGenericMethod([| sourceType; destinationType |]))
        with _ ->
            None

    let getMaps () =
        getMapper() |> Result.map (fun mapper -> mapper.ConfigurationProvider.GetAllTypeMaps())

    let find predicate =
        getMaps () |> Result.bind (Array.tryFind predicate >> Result.ofOption (NoTypeMappingFound predicate))

    let tryFind predicate =
        getMaps () |> Result.toOption |> Option.bind (Array.tryFind predicate)

