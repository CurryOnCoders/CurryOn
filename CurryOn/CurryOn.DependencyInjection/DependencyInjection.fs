namespace CurryOn.DependencyInjection

open CurryOn
open System

module DependencyInjection =
    type IServiceProvider with 
        member this.GetService<'t>() = 
            let serviceType = typeof<'t>
            try
                match this.GetService(serviceType) with
                | null -> Error <| NoServiceFound serviceType
                | :? 't as service -> Ok service
                | _ -> Error <| NoServiceFound serviceType
            with ex ->
                Error <| UnexpectedDependencyInjectionError ex

    let getService<'t> (context : IServiceProvider) = 
        if typeof<'t>.IsAssignableFrom(typeof<IServiceProvider>)
        then context |> unbox<'t> |> Ok
        else context.GetService<'t>()

    let resolve (container: IServiceProvider) (reader: Injected<_,_>) = 
        let (Reader f) = reader
        f container

    let resolveAsync (container: IServiceProvider) (reader: InjectedAsync<_,_>) = 
        async {
            let (Reader f) = reader
            return! f container |> AsyncResult.toAsync
        } |> AsyncResult

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DependencyInjectionBuilder =
    let injected<'t> = InjectionBuilder<'t>()
    let injectedAsync<'t> = AsyncInjectionBuilder<'t>()

    let inject<'t>() : Injected<'t, DependencyInjectionError> = 
        Reader (fun (context: IServiceProvider) -> DependencyInjection.getService<'t> context)

    let injectAsync<'t>() : InjectedAsync<'t, DependencyInjectionError> =
        Reader (fun (context: IServiceProvider) -> DependencyInjection.getService<'t> context |> Async.create |> AsyncResult)