namespace CurryOn.Tasks

open System
open System.Threading
open System.Threading.Tasks

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Task =
    let create<'a> (value: 'a) : Task<'a> =
        task {
            return value
        }

    let bind<'a, 'b> (f: 'a -> Task<'b>) (x: Task<'a>) : Task<'b> =
        task {
            let! value = x
            return! f value
        }

    let map<'a, 'b> (f: 'a -> 'b) (x: Task<'a>) : Task<'b> =
        task {
            let! value = x
            return f value
        }

    let ignore<'a> (t: Task<'a>) =
        t :> Task

    let private fromCompletionSource<'a> action =
        let completionSource = TaskCompletionSource<'a>()
        completionSource |> action
        completionSource.Task

    let ofException (ex: exn) =
        fromCompletionSource <| fun completionSource -> completionSource.SetException(ex)

    let cancelled<'a> () =
        fromCompletionSource<'a> <| fun completionSource -> completionSource.SetCanceled()

    let completed<'a> (value: 'a) =
        fromCompletionSource<'a> <| fun completionSource -> completionSource.SetResult(value)

    exception TaskEvaluationException of exn * AggregateException

    let ofUnit (t: Task) = 
        let completionSource = TaskCompletionSource<unit>()
        t.ContinueWith(fun (task: Task) ->
            try
                if task.IsFaulted
                then completionSource.SetException(task.Exception.Flatten())
                elif task.IsCanceled
                then completionSource.SetCanceled()
                else completionSource.SetResult()
                completionSource.Task
            finally
                task.Dispose()).Unwrap()

    let runSynchronously<'a> (task : Task<'a>) =
        task |> Async.AwaitTask |> Async.RunSynchronously

    let Parallel<'a> (tasks: Task<'a> seq) =
        Task<'a>.WhenAll(tasks)

    let sleep (time: TimeSpan) : Task =
        let task = time |> Async.Sleep |> Async.StartAsTask
        task :> Task

