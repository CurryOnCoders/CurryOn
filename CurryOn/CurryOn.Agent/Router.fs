namespace CurryOn.Agent

type RouterType<'message> =
| RoundRobin of int
| SmallestMailbox of int
| ConsistentHashing of (int * ('message -> int))

type private Routee<'message> =
    {
        Index: int
        Actor: MailboxProcessor<'message>
    }

type RouterAgent<'message> (routerType, f) =
    let createPool poolSize = 
        [| 1 .. poolSize |] |> Array.map (fun index -> MailboxProcessor<'message>.Start (f index))

    let getRoutee =
        match routerType with
        | RoundRobin poolSize ->
            let routees = createPool poolSize
            fun current _ ->
                if current >= routees.Length - 1
                then { Index = 0; Actor = routees.[0] }
                else { Index = current+1; Actor = routees.[current + 1] }
        | SmallestMailbox poolSize ->
            let routees = createPool poolSize
            fun _ _ ->
                let routee = routees |> Seq.sortBy (fun routee -> routee.CurrentQueueLength) |> Seq.head
                { Index = 0; Actor = routee }
        | ConsistentHashing (poolSize, hash) ->
            let routees = createPool poolSize
            fun _ message ->
                let index = 
                    let computedHash = hash message
                    computedHash % poolSize
                { Index = index; Actor = routees.[index] }

    let router = 
        MailboxProcessor<'message>.Start
        <| fun inbox ->
            let rec loop currentRoutee =
                async {
                    let! message = inbox.Receive()
                    let routee = message |> getRoutee currentRoutee
                    routee.Actor.Post(message)
                    return! routee.Index |> loop
                }
            loop 0

    member __.Post message =
        router.Post(message)

    member __.PostAndReply f =
        router.PostAndReply(f)

    member __.PostAndAsyncReply f =
        router.PostAndAsyncReply(f)


module RouterAgent =
    let start<'message> f routerType =
        new RouterAgent<'message>(routerType, f)
