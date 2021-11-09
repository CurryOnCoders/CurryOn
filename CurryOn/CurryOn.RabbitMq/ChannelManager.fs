namespace CurryOn.RabbitMq

open CurryOn
open RabbitMQ.Client
open System

type internal RabbitMqChannelManagerMessage =
| GetConnectionFactory of AsyncReplyChannel<IConnectionFactory>
| GetContext of AsyncReplyChannel<Result<RabbitMqContext, RabbitMqError>>

type RabbitMqChannelManager (configuration: IRabbitMqConfiguration) =    
    let primaryHost = configuration.Hosts |> Array.tryHead |> Option.defaultValue "localhost"
    let connectionFactory = ConnectionFactory(HostName = primaryHost, Port = configuration.Port, VirtualHost = configuration.VirtualHost, UserName = configuration.UserName, Password = configuration.Password)
    let prefetchCount = configuration.PrefetchCount |> Option.map uint16 |> Option.defaultValue 100us

    let createChannel (connection: IConnection) =
        try
            let channel = connection.CreateModel()
            channel.BasicQos(0ul, prefetchCount, false)
            Ok channel
        with ex ->
            Error <| RabbitMqChannelError ex.Message

    let connect () =
        result {
            try
                let connection = connectionFactory.CreateConnection(configuration.Hosts)
                let! channel = connection |> createChannel
                return { Connection = connection; Channel = channel; Properties = channel.CreateBasicProperties() }
            with ex ->
                return! Error <| RabbitMqConnectionError ex.Message
        }

    let safeDispose (disposable: IDisposable) =
        try disposable.Dispose() with _ -> ()

    let reconnect (state: RabbitMqState) =        
        state.Context |> Option.iter safeDispose
        connect ()

    let agent = 
        MailboxProcessor<RabbitMqChannelManagerMessage>.Start <| fun inbox ->
            let rec loop (state: RabbitMqState) =
                async {
                    let! message = inbox.Receive()

                    match message with
                    | GetConnectionFactory channel ->
                        channel.Reply(state.ConnectionFactory)
                        return! loop state
                    | GetContext channel ->
                        match state.Context with
                        | Some context ->
                            if context.Connection.IsOpen && context.Channel.IsOpen then
                                channel.Reply(Ok context)
                                return! loop state
                            elif context.Connection.IsOpen then
                                context.Channel |> safeDispose
                                let result = context.Connection |> createChannel
                                match result with
                                | Ok model ->
                                    let newContext = { context with Channel = model }
                                    channel.Reply(Ok newContext)
                                    return! loop { state with Context = Some newContext }
                                | Error error ->
                                    context.Connection |> safeDispose
                                    channel.Reply(Error error)
                                    return! loop { state with Context = None }
                            else
                                let result = state |> reconnect
                                match result with
                                | Ok context ->
                                    channel.Reply(Ok context)
                                    return! loop { state with Context = Some context }
                                | Error error ->
                                    channel.Reply(Error error)
                                    return! loop { state with Context = None }
                        | None ->
                            let result = connect ()
                            match result with
                            | Ok context ->
                                channel.Reply(Ok context)
                                return! loop { state with Context = Some context }
                            | Error error ->
                                channel.Reply(Error error)
                                return! loop { state with Context = None }
                }
            loop { ConnectionFactory = connectionFactory; Context = None }                

    member __.ConnectionFactory = connectionFactory :> IConnectionFactory
    
    member private __.ReturnFromContext (f) =
        task {
            let! result = agent.PostAndAsyncReply(GetContext)
            match result with
            | Ok context ->
                return f context
            | Error error ->
                return raise <| RabbitMqException(error)
        }

    member this.GetContext () =
        this.ReturnFromContext(id)
    
    member this.GetConnection () =
        this.ReturnFromContext(fun context -> context.Connection)

    member this.GetChannel () =
        this.ReturnFromContext(fun context -> context.Channel)
    
    member __.GetNewChannel () =        
        task {
            let! result = agent.PostAndAsyncReply(GetContext)
            match result |> Result.bind (fun context -> context.Connection |> createChannel) with
            | Ok channel ->
                return channel
            | Error error ->
                return raise <| RabbitMqException(error)
        }

    interface IRabbitMqChannelManager with
        member this.ConnectionFactory = this.ConnectionFactory
        member this.GetConnection () = this.GetConnection()
        member this.GetChannel () = this.GetChannel()
        member this.GetContext () = this.GetContext()
        member this.GetNewChannel () = this.GetNewChannel()