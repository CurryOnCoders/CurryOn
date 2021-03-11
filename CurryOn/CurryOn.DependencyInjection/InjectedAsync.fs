namespace CurryOn.DependencyInjection

open CurryOn
open System

/// Injection type for working with Async operations and results
type InjectedAsync<'t, 'e> = Reader<IServiceProvider, AsyncResult<'t, 'e>>

module InjectedAsync = 
    let create x = 
        Reader.create (AsyncResult.create x)
  
    let bind<'a, 'b, 'e> (f: 'a -> InjectedAsync<'b, 'e>) (x: InjectedAsync<'a, 'e>) : InjectedAsync<'b, 'e> = 
        let future state =
            async {
                let! result = Injected.run state x |> AsyncResult.toAsync
                let! futureState =
                    match result with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> Async.create |> AsyncResult
                    |> AsyncResult.toAsync
                return futureState
            } |> AsyncResult
        Reader future

    let bindAsync<'a, 'b, 'e> (f: 'a -> InjectedAsync<'b, 'e>) (x: Async<'a>) : InjectedAsync<'b, 'e> =
        let future state =
            async {
                let! result = x 
                let! future = Injected.run state (f result) |> AsyncResult.toAsync
                return future
            } |> AsyncResult
        Reader future

    let bindResult<'a, 'b, 'e> (f: 'a -> InjectedAsync<'b, 'e>) (x: Result<'a, 'e>) : InjectedAsync<'b, 'e> =
        let future state =
            async {
                let! futureState =
                    match x with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> Async.create |> AsyncResult
                    |> AsyncResult.toAsync
                return futureState
            } |> AsyncResult
        Reader future   
        
    let bindAsyncResult<'a, 'b, 'e> (f: 'a -> InjectedAsync<'b, 'e>) (x: AsyncResult<'a, 'e>) : InjectedAsync<'b, 'e> =
        let future state =
            async {
                let! result = x |> AsyncResult.toAsync
                let! futureState =
                    match result with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> Async.create |> AsyncResult
                    |> AsyncResult.toAsync
                return futureState
            } |> AsyncResult
        Reader future
        
    let bindInjected<'a, 'b, 'e> (f: 'a -> InjectedAsync<'b, 'e>) (x: Injected<'a, 'e>) : InjectedAsync<'b, 'e> =
        let future state =
            async {
                let result = x |> Reader.run state
                let! futureState =
                    match result with
                    | Ok z ->
                        Injected.run state (f z)
                    | Error e ->
                        Error e |> Async.create |> AsyncResult
                    |> AsyncResult.toAsync
                return futureState
            } |> AsyncResult
        Reader future           

    let map f x =
        bind (f >> create) x

    let mapError f (x: InjectedAsync<_,_>) : InjectedAsync<_,_> =
        let (Reader getResult) = x
        fun provider ->
            async {
                let! result = getResult provider |> AsyncResult.toAsync
                match result with
                | Ok value ->
                    return Ok value
                | Error e -> 
                    return Error (f e)
            } |> AsyncResult
        |> Reader 

    let ofAsyncResult (result: AsyncResult<_,_>) : InjectedAsync<_,_> =
        Reader.create result

    let ofAsync (result: Async<_>) : InjectedAsync<_,_> =
        result |> Async.map Ok |> AsyncResult |> ofAsyncResult

    let ofInjected (injected: Injected<_,_>) : InjectedAsync<_,_> =
        let future state =
            async {
                return injected |> Reader.run state
            } |> AsyncResult
        Reader future 

    let ofResult (result: Result<_,_>) : InjectedAsync<_,_> =
        result |> Async.create |> AsyncResult |> ofAsyncResult

    let Parallel (values: InjectedAsync<_,_> seq) : InjectedAsync<_,_> =
        let future state =
            values
            |> Seq.map (Injected.run state) 
            |> AsyncResult.Parallel
        Reader future

type AsyncInjectionBuilder<'t> () =
    member __.Bind (x, f) : InjectedAsync<_,_> = InjectedAsync.bind f x
    member __.Bind (x, f) : InjectedAsync<_,_> = InjectedAsync.bindResult f x
    member __.Bind (x, f) : InjectedAsync<_,_> = InjectedAsync.bindAsyncResult f x
    member __.Bind (x, f) : InjectedAsync<_,_> = InjectedAsync.bindAsync f x
    member __.Bind (x, f) : InjectedAsync<_,_> = InjectedAsync.bindInjected f x
    member __.Return (x) : InjectedAsync<_,_> = InjectedAsync.create x 
    member __.ReturnFrom (x: InjectedAsync<_,_>) = x    
    member __.ReturnFrom (x: AsyncResult<_,_>) = InjectedAsync.ofAsyncResult x
    member __.ReturnFrom (x: Async<_>) = InjectedAsync.ofAsync x
    member __.ReturnFrom (x: Result<_,_>) = InjectedAsync.ofResult x
    member __.ReturnFrom (x: Injected<_,_>) = InjectedAsync.ofInjected x
    member __.Zero () : InjectedAsync<_,_> = InjectedAsync.create ()
    member __.Delay (f) : InjectedAsync<_,_> = f()
    member __.Combine (a, b) : InjectedAsync<_,_> =
        a |> InjectedAsync.bind (fun () -> b)
    member this.TryWith(body : InjectedAsync<'a, 'b>, catchHandler : exn -> InjectedAsync<'a, 'b>) : InjectedAsync<'a, 'b> = 
        try
            this.ReturnFrom(body)
        with ex ->
            catchHandler ex
    member this.TryFinally(body: InjectedAsync<_,_>, compensation) : InjectedAsync<_,_> =
        try 
            this.ReturnFrom(body)
        finally 
            compensation()
    member this.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> InjectedAsync<'a, 'e>) : InjectedAsync<'a, 'e> = 
        let body =  binder resource
        this.TryFinally(body, fun () -> 
            if resource |> isNotNull
            then resource.Dispose())

    member this.While (guard, body: unit -> InjectedAsync<_,_>) : InjectedAsync<_,_> =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) : InjectedAsync<_,_> =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))
