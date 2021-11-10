namespace CurryOn.Collections

open System.Collections.Concurrent
open System.Collections.Generic

type IMessageQueue<'message> =
    abstract member Enqueue: 'message -> unit
    abstract member Dequeue: unit -> 'message option
    abstract member Length: int with get
    abstract member IsEmpty: bool with get
    abstract member HasMessages: bool with get

type IAsyncMessageQueue<'message> =
    inherit IMessageQueue<'message>
    abstract member DequeueAsync: unit -> Async<'message>
    abstract member Enqueued: IEvent<'message*int>

type MessageQueue<'message>() =
    let queue = new ConcurrentQueue<'message>()

    let enqueue message = queue.Enqueue message
    
    let dequeue () = 
        match queue.TryDequeue() with
        | (true, message) -> Some message
        | _ -> None

    let length () = queue.Count
    let isEmpty () = queue.IsEmpty
    let hasMessages () = queue.IsEmpty |> not


    member __.Enqueue message = enqueue message
    member __.Dequeue () = dequeue ()
    member __.Length with get () = length ()
    member __.IsEmpty with get () = isEmpty ()
    member __.HasMessages with get () = hasMessages ()

    interface IMessageQueue<'message> with
        member this.Enqueue message = this.Enqueue message
        member this.Dequeue () = this.Dequeue()
        member this.Length = this.Length
        member this.IsEmpty = this.IsEmpty
        member this.HasMessages = this.HasMessages

type AsyncMessageQueue<'message>() =
    let syncRoot = obj()
    let queue = ConcurrentQueue<'message>()
    let enqueued, triggerEnqueued = 
        let enqueuedEvent = Event<'message*int>()
        (enqueuedEvent.Publish, fun args -> enqueuedEvent.Trigger args)    

    let enqueue message =
        lock syncRoot <| fun () ->
            queue.Enqueue message
            triggerEnqueued (message,queue.Count)
    
    let dequeue () =
        lock syncRoot <| fun () -> 
            let rec dequeueAsync () =
                async {
                    if queue.IsEmpty
                    then do! enqueued |> Async.AwaitEvent |> Async.Ignore
                    match queue.TryDequeue() with
                    | (true, message) -> return message
                    | _ -> return! dequeueAsync ()                
                }
            dequeueAsync ()

    let tryDequeue () = 
        lock syncRoot <| fun () ->
            match queue.TryDequeue() with
            | (true, message) -> Some message
            | _ -> None

    let length () = lock syncRoot <| fun () -> queue.Count
    let isEmpty () = lock syncRoot <| fun () -> queue.IsEmpty
    let hasMessages () = lock syncRoot <| fun () -> queue.IsEmpty |> not

    member __.Enqueued = enqueued
    member __.Enqueue message = enqueue message
    member __.Dequeue () = dequeue ()
    member __.TryDequeue () = tryDequeue ()
    member __.Length with get () = length ()
    member __.IsEmpty with get () = isEmpty ()
    member __.HasMessages with get () = hasMessages ()


    interface IAsyncMessageQueue<'message> with
        member this.Enqueued = this.Enqueued
        member this.Enqueue message = this.Enqueue message
        member this.Dequeue () = this.TryDequeue ()
        member this.DequeueAsync () = this.Dequeue()
        member this.Length = this.Length
        member this.IsEmpty = this.IsEmpty
        member this.HasMessages = this.HasMessages