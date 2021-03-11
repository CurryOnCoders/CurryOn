namespace CurryOn.Mapping

open AutoMapper
open System
open System.Linq.Expressions

[<StructuredFormatDisplay("{AsString}")>]
type MappingError =
| AutoMapperInitializationFailed of exn
| NoTypeMappingFound of (TypeMap -> bool)
| ErrorMappingTypes of (Type * Type * exn)

    override this.ToString () = 
        match this with
        | AutoMapperInitializationFailed ex -> sprintf "AutoMapper initialization failed: %s" ex.Message
        | NoTypeMappingFound f -> sprintf "No type mapping found: %A" f
        | ErrorMappingTypes (t1,t2,ex) -> sprintf "Error mapping types %s -> %s: %s" t1.FullName t2.FullName ex.Message

    member this.AsString = this.ToString()


[<AutoOpen>]
module Common =
    type IMappingExpression<'source, 'destination> with
        // F# Version of AutoMapper's "ForMember" function, which expects C#-style LINQ Expressions
        member this.MapMember<'m> ( destinationGetter: Expression<Func<'destination, 'm>>,
                                    sourceGetter: Action<IMemberConfigurationExpression<'source, 'destination, 'm>> ) =
            this.ForMember(destinationGetter, sourceGetter)

    let toUtcDateTime (date: Nullable<DateTime>) =
        if date.HasValue then date.Value else DateTime.MinValue
        |> (fun localDate -> localDate.ToUniversalTime())

type OptionExpressions private () =
    static let fromOption optionalValue defaultValue getter =
        match optionalValue with
        | Some value -> value |> getter
        | None -> defaultValue
    static member Ignore<'source, 'destination, 'destinationMember> () =
        Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.Ignore())
    static member IgnoreRest<'source, 'destination> () =
        Action<IMappingExpression<'source, 'destination>>(fun (opts: IMappingExpression<'source, 'destination>) -> opts.ForAllOtherMembers(fun m -> m.Ignore()))
    static member MapFrom<'source, 'destination, 'sourceMember, 'destinationMember> (e: 'source -> 'sourceMember) =
        Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom<'sourceMember>(e))
    static member MapFromOption<'source, 'destination, 'sourceOptionMember, 'sourceTargetMember, 'destinationMember> (getOption: 'source -> 'sourceOptionMember option) defaultValue (getTarget: 'sourceOptionMember -> 'sourceTargetMember) =
        Action<IMemberConfigurationExpression<'source, 'destination, 'destinationMember>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'destinationMember>) -> opts.MapFrom<'sourceTargetMember>(fun s -> getTarget |> fromOption (getOption s) defaultValue))
    static member UseValue<'source, 'destination, 'value> (e: 'value) =
        Action<IMemberConfigurationExpression<'source, 'destination, 'value>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'value>) -> opts.MapFrom<'value>(fun _ -> e))
    static member ResolveUsing<'source, 'destination, 'value, 'resolver when 'resolver :> IValueResolver<'source, 'destination, 'value>> () = 
        Action<IMemberConfigurationExpression<'source, 'destination, 'value>> (fun (opts: IMemberConfigurationExpression<'source, 'destination, 'value>) -> opts.MapFrom<'resolver>())

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OptionExpressions =
    let mapFrom = OptionExpressions.MapFrom
    let fromOption = OptionExpressions.MapFromOption
    let ignoreMap = OptionExpressions.Ignore
    let ignoreRest = OptionExpressions.IgnoreRest
    let useValue = OptionExpressions.UseValue
    let resolve<'resolver, 'a, 'b, 'c when 'resolver :> IValueResolver<'a,'b,'c>> = OptionExpressions.ResolveUsing<'a,'b,'c,'resolver>()