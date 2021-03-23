namespace CurryOn.Agent

open FSharpx.Collections

type private PoolMessage<'a> =
| Acquire of AsyncReplyChannel<'a>
| Release of 'a

type private PoolState<'a> =
    {
        TotalCount: int
        Available: 'a list
        Waiting: Queue<AsyncReplyChannel<'a>>
    }

/// A pool of re-usable objects with Async acquire and release functions
type ObjectPool<'a>(generate: unit -> 'a, initialCount, maxCount) =
    let pool = 
        { 
            TotalCount = initialCount
            Available = List.init initialCount <| fun _ -> generate()
            Waiting = Queue.empty
        }        

    let agent = 
        MailboxProcessor.Start <| fun inbox ->
            let rec dispatch (pool: PoolState<'a>) = 
                async {
                    let! request = inbox.Receive()
                    match request with
                    | Acquire waiter ->
                        match pool.Available with
                        | head::tail ->
                            waiter.Reply(head)
                            return! dispatch { pool with Available = tail }
                        | [] ->
                            if pool.TotalCount < maxCount then 
                                let newTotal = pool.TotalCount + 1
                                waiter.Reply <| generate()
                                return! dispatch { pool with TotalCount = newTotal }
                            else 
                                return! dispatch { pool with Waiting = pool.Waiting.Conj(waiter) }
                    | Release value ->
                        match pool.Waiting |> Queue.tryUncons with
                        | Some (waiter, queue) ->
                            waiter.Reply(value)
                            return! dispatch { pool with Waiting = queue }
                        | None ->
                            return! dispatch { pool with Available = value :: pool.Available }
                }
            dispatch pool

    /// Return an item into the pool
    member __.Release (item) =
        agent.Post (Release item)

    /// Gets an item from the pool if available,
    /// or if there are none present either uses the generator if 
    /// not already at the maximum, or waits for an object to be released
    member __.Acquire () =
        agent.PostAndReply(Acquire)

    /// Asynchronously gets an item from the pool if available,
    /// or if there are none present either uses the generator if 
    /// not already at the maximum, or waits for an object to be released
    member __.AcquireAsync () =
        agent.PostAndAsyncReply(Acquire)


type PoolRouterMessage<'resource> =
| ProcessRequest of ('resource -> obj) * AsyncReplyChannel<obj>

type PoolRouterAgent<'resource> (routerType, newResource) =
    let router =
        routerType |> RouterAgent.start<PoolRouterMessage<'resource>>
            (fun _ inbox ->
                let rec loop resource =
                    async {
                        let! message = inbox.Receive()
                        match message with
                        | ProcessRequest (f, channel) ->
                            let result = f resource
                            channel.Reply(result)
                            return! loop resource
                    }
                let resource = newResource()
                loop resource)

    member __.Execute<'result> (request: 'resource -> 'result) =
        async {
            let! response = router.PostAndAsyncReply(fun channel -> ProcessRequest (request >> box, channel))
            return response |> unbox<'result>
        }

