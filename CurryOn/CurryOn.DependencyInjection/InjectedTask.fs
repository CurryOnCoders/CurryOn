namespace CurryOn.DependencyInjection

open CurryOn
open CurryOn.Tasks
open System
open System.Threading.Tasks

/// Injection type for working with Tasks and TaskResults
type InjectedTask<'t, 'e> = Reader<IServiceProvider, TaskResult<'t, 'e>>

module InjectedTask = 
    let create x = 
        Reader.create (TaskResult.create x)
  
    let bind<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: InjectedTask<'a, 'e>) : InjectedTask<'b, 'e> = 
        let future state =
            taskResult {
                let! result = Injected.run state x |> TaskResult.toAsyncResult
                let! futureState =
                    match result with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> TaskResult.ofResult
                    |> TaskResult.toAsync
                return futureState
            }
        Reader future

    let bindAsync<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: Async<'a>) : InjectedTask<'b, 'e> =
        let future state =
            taskResult {
                let! result = x 
                return! Injected.run state (f result)
            }
        Reader future

    let bindResult<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: Result<'a, 'e>) : InjectedTask<'b, 'e> =
        let future state =
            taskResult {
                let! futureState =
                    match x with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> TaskResult.ofResult
                    |> TaskResult.toAsync
                return futureState
            }
        Reader future 

    let bindAsyncResult<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: AsyncResult<'a, 'e>) : InjectedTask<'b, 'e> =
        let future state =
            taskResult {
                let (AsyncResult asyncValue) = x
                let! result = asyncValue
                let! futureState =
                    match result with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> TaskResult.ofResult
                    |> TaskResult.toAsync
                return futureState
            }
        Reader future
        
    let bindTask<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: Task<'a>) : InjectedTask<'b, 'e> =
        let future state =
            taskResult {
                let! result = x
                return! Injected.run state (f result)
            }
        Reader future

    let bindTaskResult<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: TaskResult<'a, 'e>) : InjectedTask<'b, 'e> =
        let future state =
            taskResult {
                let! result = x                
                return! Injected.run state (f result)
            }
        Reader future
        
    let bindInjected<'a, 'b, 'e> (f: 'a -> InjectedTask<'b, 'e>) (x: Injected<'a, 'e>) : InjectedTask<'b, 'e> =
        let future state =
            taskResult {
                let result = x |> Reader.run state
                let! futureState =
                    match result with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> TaskResult.ofResult
                    |> TaskResult.toAsync
                return futureState
            }
        Reader future           

    let map f x =
        bind (f >> create) x

    let mapError f (x: InjectedTask<_,_>) : InjectedTask<_,_> =
        let (Reader getResult) = x
        fun provider ->
            taskResult {
                let! result = getResult provider |> TaskResult.toAsync
                match result with
                | Ok value ->
                    return Ok value
                | Error e -> 
                    return Error (f e)
            }
        |> Reader 

    let ofTask (t: Task<_>) : InjectedTask<_,_> =
        t |> TaskResult.ofTask |> Reader.create

    let ofTaskResult (result: TaskResult<_,_>) : InjectedTask<_,_> =
        Reader.create result

    let ofAsync (result: Async<_>) : InjectedTask<_,_> =
        result |> Async.map Ok |> TaskResult.ofAsyncResult |> ofTaskResult

    let ofAsyncResult (result: AsyncResult<_,_>) : InjectedTask<_,_> =
        let (AsyncResult asyncValue) = result
        asyncValue |> TaskResult.ofAsyncResult |> ofTaskResult

    let ofInjected (injected: Injected<_,_>) : InjectedTask<_,_> =
        let future state =
            taskResult {
                return injected |> Reader.run state
            }
        Reader future 

    let ofResult (result: Result<_,_>) : InjectedTask<_,_> =
        result |> TaskResult.ofResult |> ofTaskResult

    let Parallel (values: InjectedTask<_,_> seq) : InjectedTask<_,_> =
        let future state =
            values
            |> Seq.map (Injected.run state) 
            |> TaskResult.Parallel
        Reader future

type TaskInjectionBuilder<'t> () =
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bind f x
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bindResult f x    
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bindTask f x
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bindTaskResult f x
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bindAsync f x
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bindAsyncResult f x
    member __.Bind (x, f) : InjectedTask<_,_> = InjectedTask.bindInjected f x
    member __.Return (x) : InjectedTask<_,_> = InjectedTask.create x 
    member __.ReturnFrom (x: InjectedTask<_,_>) = x    
    member __.ReturnFrom (x: Task<_>) = InjectedTask.ofTask x
    member __.ReturnFrom (x: TaskResult<_,_>) = InjectedTask.ofTaskResult x
    member __.ReturnFrom (x: Async<_>) = InjectedTask.ofAsync x
    member __.ReturnFrom (x: Result<_,_>) = InjectedTask.ofResult x
    member __.ReturnFrom (x: AsyncResult<_,_>) = InjectedTask.ofAsyncResult x
    member __.ReturnFrom (x: Injected<_,_>) = InjectedTask.ofInjected x
    member __.Zero () : InjectedTask<_,_> = InjectedTask.create ()
    member __.Delay (f) : InjectedTask<_,_> = f()
    member __.Combine (a, b) : InjectedTask<_,_> =
        a |> InjectedTask.bind (fun () -> b)
    member this.TryWith(body : InjectedTask<'a, 'b>, catchHandler : exn -> InjectedTask<'a, 'b>) : InjectedTask<'a, 'b> = 
        try
            this.ReturnFrom(body)
        with ex ->
            catchHandler ex
    member this.TryFinally(body: InjectedTask<_,_>, compensation) : InjectedTask<_,_> =
        try 
            this.ReturnFrom(body)
        finally 
            compensation()
    member this.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> InjectedTask<'a, 'e>) : InjectedTask<'a, 'e> = 
        let body =  binder resource
        this.TryFinally(body, fun () -> 
            if resource |> isNotNull
            then resource.Dispose())

    member this.While (guard, body: unit -> InjectedTask<_,_>) : InjectedTask<_,_> =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) : InjectedTask<_,_> =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))
