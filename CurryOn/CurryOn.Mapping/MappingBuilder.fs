namespace CurryOn.Mapping

open AutoMapper
open FSharp.Linq.RuntimeHelpers
open FSharp.Quotations
open System
open System.Linq.Expressions

type MappingProfile () =
    inherit Profile()

type MappingProfileBuilder() =
    let mutable profile = Unchecked.defaultof<MappingProfile>
    let toLinq (expr : Expr<'a -> 'b>) =
        let linq = expr |> LeafExpressionConverter.QuotationToExpression
        let call = linq |> unbox<MethodCallExpression>
        let lambda = call.Arguments.[0] |> unbox<LambdaExpression>
        Expression.Lambda<Func<'a, 'b>>(lambda.Body, lambda.Parameters) 
    member __.Yield x = 
        profile <- new MappingProfile()
        profile
    [<CustomOperation("converter")>]
    member __.Converter<'source, 'destination> (profile: MappingProfile, c: ITypeConverter<'source, 'destination>) =
        let map = profile.CreateMap<'source, 'destination>()
        map.ConvertUsing(c)
        profile
    [<CustomOperation("oneway")>]
    member __.ConvertOneWay<'source, 'destination> (profile: MappingProfile, f: 'source -> 'destination) =
        let map = profile.CreateMap<'source, 'destination>()
        map.ConvertUsing({new ITypeConverter<'source, 'destination> with member __.Convert (source,_,_) = f source})
        profile
    [<CustomOperation("twoway")>]
    member __.ConvertTwoWay<'a, 'b> (profile: MappingProfile, f: 'a -> 'b, g: 'b -> 'a) =
        let mapAB = profile.CreateMap<'a, 'b>()
        let mapBA = profile.CreateMap<'b, 'a>()
        mapAB.ConvertUsing({new ITypeConverter<'a, 'b> with member __.Convert (source,_,_) = f source})
        mapBA.ConvertUsing({new ITypeConverter<'b, 'a> with member __.Convert (source,_,_) = g source})
        profile
    member __.Run (_) =
        profile :> Profile

type MappingProfileBuilder<'source, 'destination> () =
    let mutable profile = Unchecked.defaultof<MappingProfile>
    let toLinq (expr : Expr<'a -> 'b>) =
        let linq = expr |> LeafExpressionConverter.QuotationToExpression
        let call = linq |> unbox<MethodCallExpression>
        let lambda = call.Arguments.[0] |> unbox<LambdaExpression>
        Expression.Lambda<Func<'a, 'b>>(lambda.Body, lambda.Parameters) 
    member __.Yield x = 
        profile <- new MappingProfile()
        profile.CreateMap<'source, 'destination>()
    member __.Destination = Unchecked.defaultof<'destination>
    [<CustomOperation("map")>]
    member __.Map (map: IMappingExpression<'source, 'destination>, [<ReflectedDefinition>] s: Expr<'source -> 'sourceMember>, [<ReflectedDefinition>] d: Expr<'destination -> 'destinationMember>) =
        let mapTo = d |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom<'sourceMember>(s |> toLinq))
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("mapDefault")>]
    member __.MapNotNull (map: IMappingExpression<'source, 'destination>, [<ReflectedDefinition>] s: Expr<'source -> 'sourceMember>, [<ReflectedDefinition>] d: Expr<'destination -> 'destinationMember>, defaultValue: 'sourceMember when 'sourceMember : null) =
        let mapTo = d |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom<'sourceMember>(s |> toLinq); opts.NullSubstitute(defaultValue))
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("mapOption")>]
    member __.MapOption (map: IMappingExpression<'source, 'destination>, [<ReflectedDefinition>] s: Expr<'source -> 'sourceMember>, [<ReflectedDefinition>] d: Expr<'destination -> 'destinationMember>, defaultValue: 'sourceMember when 'sourceMember : null) =
        let mapTo = d |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom<'sourceMember>(s |> toLinq); opts.NullSubstitute(defaultValue))
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("ignore")>]
    member __.Ignore (map: IMappingExpression<'source, 'destination>, [<ReflectedDefinition>] x: Expr<'destination -> 'destintaionMember>) = 
        let mapTo = x |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destintaionMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destintaionMember>) -> opts.Ignore())
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("value")>]
    member __.Value (map: IMappingExpression<'source, 'destination>, v: 'destinationMember, [<ReflectedDefinition>] x: Expr<'destination -> 'destinationMember>) =
        let mapTo = x |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom(v))
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("resolve")>]
    member __.Resolve (map: IMappingExpression<'source, 'destination>, r: IValueResolver<'source, 'destination, 'destinationMember>, [<ReflectedDefinition>] x: Expr<'destination -> 'destinationMember>) =
        let mapTo = x |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom(r))
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("mapArray")>]
    member this.MapArray (map: IMappingExpression<'source, 'destination>, getSourceMember: 'source -> 'sourceMember [],  [<ReflectedDefinition>] x: Expr<'destination -> 'destinationMember []>) =
        let resolver = 
            {new IValueResolver<'source, 'destination, 'destinationMember []> with
                member __.Resolve(source, _, _, context) =
                    let sourceMembers = source |> getSourceMember
                    sourceMembers |> Array.map context.Mapper.Map<'sourceMember, 'destinationMember>
            }
        let mapTo = x |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember[]>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember[]>) -> opts.MapFrom(resolver))
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("converter")>]
    member __.Converter (map: IMappingExpression<'source, 'destination>, c: ITypeConverter<'source, 'destination>) =
        map.ConvertUsing(c)
        map
    [<CustomOperation("convertWith")>]
    member __.ConvertWith (map: IMappingExpression<'source, 'destination>, f: 'source -> 'destination) =
        map.ConvertUsing({new ITypeConverter<'source, 'destination> with member __.Convert (source,_,_) = f source})
        map
    [<CustomOperation("convert")>]
    member __.Convert (map: IMappingExpression<'source, 'destination>, [<ReflectedDefinition>] s: Expr<'source -> 'sourceMember>, [<ReflectedDefinition>] d: Expr<'destination -> 'destinationMember>, f: 'sourceMember -> 'destinationMember) =
        let mapTo = d |> toLinq
        let mapFrom = Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom<'sourceMember>(<@ fun source -> source |> %s @> |> toLinq))                 
        profile.CreateMap<'sourceMember, 'destinationMember>().ConvertUsing({new ITypeConverter<'sourceMember, 'destinationMember> with member __.Convert (source,_,_) = f source})
        map.ForMember(mapTo, mapFrom)
    [<CustomOperation("ignoreRest")>]
    member __.IgnoreRest (map: IMappingExpression<'source, 'destination>) =
        map.ForAllOtherMembers(Action<IMemberConfigurationExpression<'source, 'destination, obj>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, obj>) -> opts.Ignore()))
        map
    member __.Run (_) =
        profile :> Profile

[<AutoOpen>]
module MapBuilder =
    let autoconvert = MappingProfileBuilder()
    let inline automapper<'a,'b> = MappingProfileBuilder<'a,'b>()