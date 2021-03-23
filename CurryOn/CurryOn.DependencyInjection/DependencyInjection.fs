namespace CurryOn.DependencyInjection

open CurryOn
open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DependencyInjection =
    type IServiceProvider with 
        member this.TryGetService<'t>() = 
            let serviceType = typeof<'t>
            try
                match this.GetService(serviceType) with
                | null -> None
                | :? 't as service -> Some service
                | _ -> None
            with _ ->
                None
        member this.GetService<'t>() = 
            let serviceType = typeof<'t>
            try
                match this.GetService(serviceType) with
                | null -> Error <| NoServiceFound serviceType
                | :? 't as service -> Ok service
                | _ -> Error <| NoServiceFound serviceType
            with ex ->
                Error <| UnexpectedDependencyInjectionError ex

    let tryGetService<'t> (context : IServiceProvider) = 
        if typeof<'t>.IsAssignableFrom(typeof<IServiceProvider>)
        then context |> unbox<'t> |> Some
        else context.TryGetService<'t>()

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


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Container =
    type private ContainerMessage =
    | SetProvider of IServiceProvider
    | GetProvider of AsyncReplyChannel<IServiceProvider option>

    let private agent =
        MailboxProcessor<ContainerMessage>.Start <| fun inbox ->
            let rec loop (provider: IServiceProvider option) =
                async {
                    let! message = inbox.Receive()
                    match message with
                    | SetProvider serviceProvider ->
                        return! loop (Some serviceProvider)
                    | GetProvider channel ->
                        channel.Reply(provider)
                        return! loop provider
                }
            loop None
    
    let set provider =
        agent.Post (SetProvider provider)

    let get () =
        agent.PostAndAsyncReply GetProvider

    let instanceOf<'service>() = 
        async {
            let! provider = get()
            return provider |> Option.bind (fun container -> container |> DependencyInjection.tryGetService<'service>)
        }