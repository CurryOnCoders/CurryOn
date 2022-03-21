namespace CurryOn.Reliable

type ReliabilityError =
| TransientError of string
| InstanceEndingError of string
| UnexpectedException of exn

type ReliablilityState<'t> = 
    private {
        Factory: unit -> Result<'t, ReliabilityError>
        ErrorNotification: Event<ReliabilityError>
        Cleanup: 't -> unit
        CurrentInstance: 't option
    } member this.Error = this.ErrorNotification.Publish

type private ReliabilityMessage<'t> =
| ProcessCommand of ('t -> Result<obj, ReliabilityError>) * AsyncReplyChannel<Result<obj, ReliabilityError>>
| Shutdown of AsyncReplyChannel<unit>

type Reliable<'t> (initialState: ReliablilityState<'t>) =
    let agent =
        MailboxProcessor<ReliabilityMessage<'t>>.Start <| fun inbox ->
            let rec loop (state: ReliablilityState<'t>) =
                async {
                    let! message = inbox.Receive()
                    match message with
                    | ProcessCommand (action, channel) ->
                        match state.CurrentInstance with
                        | Some instance ->
                            try
                                let result = action instance
                                match result with
                                | Ok value ->
                                    channel.Reply(result)
                                | Error error ->
                                    state.ErrorNotification.Trigger(error)
                                    channel.Reply()
                                    match error with
                                    | TransientError _ ->
                                return! loop state
                            with ex ->
                                
                            
                }
            loop initialState