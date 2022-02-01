namespace CurryOn.Tasks

open System
open System.Threading.Tasks

type TaskResultException<'error> (error: 'error) =
    inherit Exception(sprintf "%A" error)
    member __.Error = error

[<NoComparison; NoEquality>]
type TaskResult<'a, 'e> = 
    | TaskResult of Task<Result<'a, 'e>>
    member this.GetAwaiter () =        
        let (TaskResult t) = this
        let unwrapped = 
            backgroundTask {
                let! result = t
                match result with
                | Ok value ->
                    return value
                | Error error ->
                    return (raise <| TaskResultException error)
            }
        unwrapped.GetAwaiter()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TaskResult =
    /// Create an TaskResult from an ordinary value
    let create value =
        value |> Ok |> Task.create |> TaskResult        

    /// Convert Task<'a> to TaskResult<'a, 'e>
    let ofTask<'a, 'e> (t: Task<'a>) : TaskResult<'a, 'e> =
        backgroundTask {
            let! value = t.ConfigureAwait(false)
            return Ok value
        } |> TaskResult

    /// Convert TaskResult<'a, 'e> to Task<'a>
    let toTask<'a, 'e> (TaskResult (t: Task<Result<'a, 'e>>)) = 
        task {
            let! result = t
            match result with
            | Ok value ->
                return value
            | Error e ->
                return raise <| TaskResultException<'e>(e)
        }

    /// Convert TaskResult<'a, 'e> to Task<'a>
    let toBackgroundTask<'a, 'e> (TaskResult (t: Task<Result<'a, 'e>>)) = 
        backgroundTask {
            let! result = t
            match result with
            | Ok value ->
                return value
            | Error e ->
                return raise <| TaskResultException<'e>(e)
        }

    /// Convert Task<Result<'a, 'e>> to TaskResult<'a, 'e>
    let wrap<'a, 'e> (task: Task<Result<'a,'e>>) =
        TaskResult task

    /// Convert TaskResult<'a, 'e> to Task<Result<'a, 'e>>
    let unwrap<'a, 'e> (TaskResult (task: Task<Result<'a,'e>>)) =
        task

    /// Convert Async<Result<'a, 'e>> to TaskResult<'a, 'e>
    let ofAsync<'a, 'e> (a: Async<'a>) : TaskResult<'a, 'e> =
        backgroundTask {
            let! value = a |> Async.StartAsTask
            return Ok value
        } |> TaskResult 

    /// Convert TaskResult<'a, 'e> to Async<Result<'a, 'e>>
    let toAsync (TaskResult task) = 
        async {
            let! result = task |> Async.AwaitTask
            match result with
            | Ok value ->
                return value
            | Error error ->
                return (raise <| TaskResultException error)
        }

    /// Convert Result<'a, 'e> to TaskResult<'a, 'e>
    let ofResult (result: Result<'a, 'e>) : TaskResult<'a, 'e> =
        let source = TaskCompletionSource()
        source.SetResult(result)
        source.Task |> TaskResult

    /// Convert TaskResult<'a, 'e> to Result<'a, 'e>
    let toResult (TaskResult taskResult) = 
        taskResult |> Async.AwaitTask |> Async.RunSynchronously

    /// Convert Async<Result<'a, 'e>> to TaskResult<'a, 'e>
    let ofAsyncResult (asyncResult: Async<Result<'a, 'e>>) =
        asyncResult |> Async.StartAsTask |> TaskResult 

    /// Convert TaskResult<'a, 'e> to Async<Result<'a, 'e>>
    let toAsyncResult (TaskResult result) = result |> Async.AwaitTask

    /// Standard monadic bind on TaskResult
    /// Effectively composes Task.bind and Result.bind
    let bind binder taskResult =
        let fSuccess value = 
            value |> (binder >> unwrap)
            
        let fFailure errs = 
            errs |> Error |> Task.create

        let either a b = function
        | Ok v -> a v
        | Error e -> b e            
            
        taskResult
        |> unwrap
        |> Task.bind (either fSuccess fFailure)
        |> TaskResult

    /// Extract the value from an TaskResult and run the following function,
    /// returning a new TaskResult if successful, or the original error if not
    let map f x = 
        backgroundTask {
            let! result = x |> unwrap
            match result with
            | Ok value -> 
                return Ok <| f value
            | Error error -> 
                return Error error
        } |> TaskResult

    /// Map the Result inside an TaskResult, without unwrapping the inner value
    let mapResult f = unwrap >> Task.map f >> TaskResult

    /// Map error type of an TaskResult<'c, 'a> to TaskResult<'c, 'b>
    let mapError f = mapResult <| Result.mapError f

    /// Merge multiple Async Results into a single result with a list of any errors
    let join (results: TaskResult<'a, 'e> seq) =
        results |> Seq.fold (fun acc cur ->
            backgroundTask {
                let! accumulator = acc |> unwrap
                let! current = cur |> unwrap
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
            } |> TaskResult) (Task.create (Ok []) |> TaskResult)

    /// Parallelizes the execution of multiple TaskResults and 
    /// returns a single TaskResult with a list of the results or errors
    let Parallel computations =
        let join results =
            results |> Seq.fold (fun acc cur ->
                match acc with
                | Ok values -> 
                    match cur with
                    | Ok value -> Ok (values @ [value])
                    | Error error -> Error [error]
                | Error errors ->
                    match cur with
                    | Error error -> Error (error :: errors)
                    | _ -> Error errors) (Ok [])

        backgroundTask {
            let! results =
                computations
                |> Seq.map unwrap
                |> Task.WhenAll

            return results |> join
        } |> TaskResult

    let wait (TaskResult task) =
        task |> Async.AwaitTask |> Async.Ignore |> Async.RunSynchronously

type TaskResultBuilder () =
    member __.Return value : TaskResult<'a, 'b> = 
        TaskResult.create value

    member __.ReturnFrom(taskResult : TaskResult<'a, 'b>) = 
        taskResult

    member __.ReturnFrom(task : Task<'a>) = 
        task |> TaskResult.ofTask

    member __.ReturnFrom(a : Async<'a>) = 
        a |> TaskResult.ofAsync

    member __.ReturnFrom(asyncResult : Async<Result<'a, 'e>>) = 
        asyncResult |> TaskResult.ofAsyncResult

    member __.ReturnFrom(r: Result<'a, 'e>) : TaskResult<'a, 'e> =
        r |> TaskResult.ofResult

    member this.Zero() : TaskResult<unit, 'b> = 
        this.Return()

    member __.Delay(generator : unit -> TaskResult<'a, 'b>) : TaskResult<'a, 'b> = 
        let t =
            backgroundTask {
                return generator
            }
        t.ContinueWith(fun (t: Task<unit -> TaskResult<'a, 'b>>) -> t.Result () |> TaskResult.unwrap).Unwrap()
        |> TaskResult
    
    member __.Bind(taskResult : TaskResult<'a, 'c>, binder : 'a -> TaskResult<'b, 'c>) : TaskResult<'b, 'c> = 
        TaskResult.bind binder taskResult
    
    member this.Bind(result : Result<'a, 'c>, binder : 'a -> TaskResult<'b, 'c>) : TaskResult<'b, 'c> = 
        this.Bind(result |> TaskResult.ofResult, binder)

    member __.Bind(t : Task<'a>, binder : 'a -> TaskResult<'b, 'c>) : TaskResult<'b, 'c> = 
        t |> TaskResult.ofTask |> TaskResult.bind binder
    
    member this.Bind(async : Async<'a>, binder : 'a -> TaskResult<'b, 'c>) : TaskResult<'b, 'c> = 
        this.Bind(async |> TaskResult.ofAsync, binder)

    member this.Bind(async : Async<Result<'a, 'e>>, binder : 'a -> TaskResult<'b, 'e>) : TaskResult<'b, 'e> = 
        this.Bind(async |> TaskResult.ofAsyncResult, binder)

    member __.Combine (a, b) = TaskResult.bind (fun () -> b) a
    
    member __.TryWith(taskResult : TaskResult<'a, 'b>, catchHandler : exn -> TaskResult<'a, 'b>) : TaskResult<'a, 'b> = 
        backgroundTask {
            try
                let! result = taskResult |> TaskResult.unwrap
                return result
            with ex ->
                let! error = catchHandler ex |> TaskResult.unwrap
                return error
        } |> TaskResult

    member __.TryFinally(taskResult : TaskResult<'a, 'b>, compensation : unit -> unit) : TaskResult<'a, 'b> = 
        backgroundTask {
            try
                return! taskResult |> TaskResult.unwrap
            finally
                compensation ()
        } |> TaskResult

    member __.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> TaskResult<'a, 'b>) : TaskResult<'a, 'b> = 
        backgroundTask {
            use d = resource
            return! binder d |> TaskResult.unwrap
        } |> TaskResult

    member this.While (guard, body: unit -> TaskResult<_,_>) =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))
            
[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TaskResultBuilder =
    let taskResult = TaskResultBuilder()

    type Task<'t> with
        member task.Map(f) =
            task |> Task.map f