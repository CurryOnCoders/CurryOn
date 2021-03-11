namespace CurryOn

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<NoComparison; NoEquality>]
type AsyncResult<'a, 'e> = AsyncResult of Async<Result<'a, 'e>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    let create x =
        async.Return(x)

    let bind f x =
        async.Bind(x, f)

    let map f x =
        async.Bind(x, f >> create)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncResult =
    type AsyncOperationFailedException<'error> (error: 'error) =
        inherit Exception(sprintf "%A" error)
        member __.Error = error

    /// Create an Async Result from a value
    let create value =
        async {
            return Ok value
        } |> AsyncResult

    /// Convert Async<Result<'a, 'e>> to AsyncResult<'a, 'e>
    let ofAsync (asyncResult: Async<Result<'a, 'e>>) =
        AsyncResult asyncResult

    /// Convert AsyncResult<'a, 'e> to Async<Result<'a, 'e>>
    let toAsync (AsyncResult result) = result

    /// Convert Result<'a, 'e> to AsyncResult<'a, 'e>
    let ofResult (result: Result<'a, 'e>) =
        result |> Async.create |> AsyncResult

    /// Convert AsyncResult<'a, 'e> to Result<'a, 'e>
    let toResult result = 
        result |> toAsync |> Async.RunSynchronously

    /// Standard monadic bind on AsyncResult
    /// Effectively composes Async.bind and Result.bind
    let bind binder asyncResult =
        let fSuccess value = 
            value |> (binder >> toAsync)
            
        let fFailure errs = 
            errs |> Error |> Async.create
            
        asyncResult
        |> toAsync
        |> Async.bind (Result.either fSuccess fFailure)
        |> AsyncResult

    /// Extract the value from an AsyncResult and run the following function,
    /// returning a new AsyncResult if successful, or the original error if not
    let map f x = 
        async {
            let! result = x |> toAsync
            match result with
            | Ok value -> 
                return Ok <| f value
            | Error error -> 
                return Error error
        } |> AsyncResult

    /// Map the Result inside an AsyncResult, without unwrapping the inner value
    let mapResult f = toAsync >> Async.map f >> AsyncResult

    /// Map error type of an AsyncResult<'c, 'a> to AsyncResult<'c, 'b>
    let mapError f = mapResult <| Result.mapError f

    /// Merge multiple Async Results into a single result with a list of any errors
    let join (results: AsyncResult<'a, 'e> seq) =
        results |> Seq.fold (fun acc cur ->
            async {
                let! accumulator = acc |> toAsync
                let! current = cur |> toAsync
                return
                    match accumulator with
                    | Ok values ->                     
                        match current with
                        | Ok value -> Ok (values @ [value])
                        | Error error -> Error [error]
                    | Error errors ->
                        match current with
                        | Error error -> Error (error :: errors)
                        | _ -> Error errors
            } |> AsyncResult) (Async.create (Ok []) |> AsyncResult)

    /// Convert Task<'a> to AsyncResult<'a, 'e>
    let ofTask<'a, 'error> (convertExceptionToError: exn -> 'error) (task: Task<'a>) =
        async {
            try
                let! value = task |> Async.AwaitTask
                return Ok value
            with ex ->
                return Error <| convertExceptionToError ex
        } |> AsyncResult

    /// Convert AsyncResult<'a, 'e> to Task<'a>
    let toTask (asyncResult: AsyncResult<'a, 'e>) =
        async {
            let! result = asyncResult |> toAsync

            let value =
                match result with
                | Ok value -> value                    
                | Error error -> raise <| AsyncOperationFailedException(error)

            return value
        } |> Async.StartAsTask

    /// Parallelizes the execution of multiple AsyncResults and 
    /// returns a single AsyncResult with a list of the results or errors
    let Parallel computations =
        async {
            let! results =
                computations
                |> Seq.map toAsync
                |> Async.Parallel

            return results |> Result.join
        } |> AsyncResult
        

[<Extension>]
type TaskExtensions =
    [<Extension>]
    static member ToAsyncResult<'a, 'error>(task: Task<'a>, convertExceptionToError: Func<exn, 'error>) =
        task |> AsyncResult.ofTask (fun ex -> convertExceptionToError.Invoke(ex))

    [<Extension>]
    static member ToAsyncResult<'error>(task: Task, convertExceptionToError: Func<exn, 'error>) =
        task |> Async.AwaitTask |> Async.StartAsTask |> AsyncResult.ofTask (fun ex -> convertExceptionToError.Invoke(ex))

    [<Extension>]
    static member ToTask<'a, 'error>(result: AsyncResult<'a, 'error>) =
        result |> AsyncResult.toTask

type AsyncResultBuilder() = 
    member __.Return value : AsyncResult<'a, 'b> = 
        value
        |> Ok
        |> Async.create
        |> AsyncResult        

    member __.ReturnFrom(asyncResult : AsyncResult<'a, 'b>) = 
        asyncResult

    member __.ReturnFrom(a : Async<'a>) = 
        a |> Async.map Ok |> AsyncResult.ofAsync

    member __.ReturnFrom(r: Result<'a, 'b>) =
        r |> Async.create |> AsyncResult.ofAsync

    member this.Zero() : AsyncResult<unit, 'b> = 
        this.Return()

    member __.Delay(generator : unit -> AsyncResult<'a, 'b>) : AsyncResult<'a, 'b> = 
        async.Delay(generator >> AsyncResult.toAsync) |> AsyncResult
    
    member __.Bind(asyncResult : AsyncResult<'a, 'c>, binder : 'a -> AsyncResult<'b, 'c>) : AsyncResult<'b, 'c> = 
        AsyncResult.bind binder asyncResult
    
    member this.Bind(result : Result<'a, 'c>, binder : 'a -> AsyncResult<'b, 'c>) : AsyncResult<'b, 'c> = 
        this.Bind(result |> Async.create |> AsyncResult, binder)
    
    member __.Bind(async : Async<'a>, binder : 'a -> AsyncResult<'b, 'c>) : AsyncResult<'b, 'c> = 
        async
        |> Async.bind (binder >> AsyncResult.toAsync)
        |> AsyncResult

    member __.Combine (a, b) = AsyncResult.bind (fun () -> b) a
    
    member __.TryWith(asyncResult : AsyncResult<'a, 'b>, catchHandler : exn -> AsyncResult<'a, 'b>) : AsyncResult<'a, 'b> = 
        async.TryWith(asyncResult |> AsyncResult.toAsync, (catchHandler >> AsyncResult.toAsync)) |> AsyncResult

    member __.TryFinally(asyncResult : AsyncResult<'a, 'b>, compensation : unit -> unit) : AsyncResult<'a, 'b> = 
        async.TryFinally(asyncResult |> AsyncResult.toAsync, compensation) |> AsyncResult

    member __.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> AsyncResult<'a, 'b>) : AsyncResult<'a, 'b> = 
        async.Using(resource, (binder >> AsyncResult.toAsync)) |> AsyncResult

    member this.While (guard, body: unit -> AsyncResult<_,_>) =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EitherAsyncMonad =
    let asyncResult = AsyncResultBuilder()