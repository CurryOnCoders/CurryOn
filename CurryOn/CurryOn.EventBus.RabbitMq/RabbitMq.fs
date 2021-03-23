namespace CurryOn.EventBus.RabbitMq

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Domain
open CurryOn.EventBus
open CurryOn.Serialization
open Newtonsoft.Json.Linq
open RabbitMQ.Client
open System
open System.Collections.Generic

module internal RabbitMq =
    type private RabbitMqConnectionMessage =
    | Connect of IRabbitMqConnection * AsyncReplyChannel<Result<IConnection, RabbitMqError>>
    | Disconnect of AsyncReplyChannel<unit>

    type private RabbitMqChannelMessage =
    | GetChannel of IRabbitMqConnection * IRabbitMqChannel * AsyncReplyChannel<Result<IModel * IBasicProperties, RabbitMqError>>

    type private RabbitMqConsumerMessage =
    | AddConsumer of string * string
    | GetConsumer of string * AsyncReplyChannel<string option>
    | RemoveConsumer of string

    type private RabbitMqConnectionState =
        {
            ConnectionFactory: IConnectionFactory option
            Connection: IConnection option
        }

    let private connectionManager =
        MailboxProcessor<RabbitMqConnectionMessage>.Start <| fun inbox ->
            let connect (state: RabbitMqConnectionState) (connectionInfo: IRabbitMqConnection) =
                try
                    match state.ConnectionFactory, state.Connection with
                    | Some factory, Some connection ->
                        Ok (factory, connection)
                    | Some factory, _ ->
                        let connection = factory.CreateConnection(connectionInfo.Servers)
                        Ok (factory, connection)
                    | _,_ ->
                        let server = connectionInfo.Servers |> Seq.head
                        let factory = ConnectionFactory(UserName = connectionInfo.UserName, Password = connectionInfo.Password, HostName = server, VirtualHost = "/", DispatchConsumersAsync = true) :> IConnectionFactory
                        let connection = factory.CreateConnection(connectionInfo.Servers)
                        Ok (factory, connection)
                with ex ->
                    Error <| RabbitConnectionError (ex.Message, connectionInfo)

            let rec loop (state: RabbitMqConnectionState) =
                async {
                    let! message = inbox.Receive()
                    match message with
                    | Connect (connectionInfo, replyChannel) ->
                        let result = connect state connectionInfo
                        match result with
                        | Ok (factory, connection) ->
                            replyChannel.Reply(Ok connection)
                            return! loop { state with ConnectionFactory = Some factory; Connection = Some connection }
                        | Error error ->
                            replyChannel.Reply(Error error)
                            return! loop state
                    | Disconnect replyChannel ->
                        match state.Connection with
                        | Some connection ->
                            connection.Dispose()
                            replyChannel.Reply()
                            return! loop { state with Connection = None }
                        | None ->
                            replyChannel.Reply()
                            return! loop state
                    return! loop state
                }
            loop { ConnectionFactory = None; Connection = None }

    let private channelManager =
        MailboxProcessor<RabbitMqChannelMessage>.Start <| fun inbox ->
            let rec loop () =
                async {
                    let! message = inbox.Receive()
                    match message with
                    | GetChannel (connectionInfo, channelInfo, replyChannel) ->
                        let! connectionResult = connectionManager.PostAndAsyncReply(fun reply -> Connect (connectionInfo, reply))
                        match connectionResult with
                        | Ok connection ->
                            try
                                let channel = connection.CreateModel()
                                channel.ExchangeDeclare(channelInfo.Exchange, ExchangeType.Topic, true, false)                                
                                let properties = channel.CreateBasicProperties()
                                replyChannel.Reply(Ok (channel, properties))                                
                            with ex ->
                                replyChannel.Reply(Error <| RabbitChannelError ex.Message)
                        | Error error ->
                            replyChannel.Reply(Error error)
                    return! loop ()
                }
            loop ()

    let private consumerManager =
        MailboxProcessor<RabbitMqConsumerMessage>.Start <| fun inbox ->
            let rec loop consumers =
                async {
                    let! message = inbox.Receive()
                    match message with
                    | AddConsumer (queue, consumerTag) ->
                        let state = consumers |> Map.add queue consumerTag
                        return! loop state
                    | GetConsumer (queue, channel) ->
                        let consumer = consumers |> Map.tryFind queue
                        channel.Reply(consumer)
                        return! loop consumers
                    | RemoveConsumer queue ->
                        let state = consumers |> Map.remove queue
                        return! loop state                        
                }
            loop Map.empty

    let connect connectionInfo =
        asyncResult {
            let! result = connectionManager.PostAndAsyncReply(fun reply -> Connect(connectionInfo, reply))
            return! result
        }

    let connectChannel connectionInfo channelInfo =
        asyncResult {
            let! result = channelManager.PostAndAsyncReply(fun reply -> GetChannel (connectionInfo, channelInfo, reply))
            return! result
        }

    let send connectionInfo channelInfo (message: IEvent) =
        injectedAsync {
            let! serializer = injectAsync<IJsonSerializer>() |> InjectedAsync.mapError DependencyInjectionFailure
            let! (channel, properties) = connectChannel connectionInfo channelInfo
            try
                let! bytes = message |> serializer.Serialize |> Result.mapError SerializationError |> Result.map ReadOnlyMemory
                try 
                    channel.BasicPublish(channelInfo.Exchange, channelInfo.RoutingKey, properties, bytes)
                with ex -> 
                    return! Error <| RabbitPublishingError (message, ex.Message)
            finally
                channel.Dispose()
        }        

    let private getEvents = 
        let allEvents = lazy (Types.filter (fun t -> t.Implements<IEvent>()) |> Seq.toList)
        fun () -> !allEvents

    let findEventType (name: string) =
        let eventTypes = getEvents()        
        match eventTypes |> List.tryFind (fun t -> t.Name |> String.like name) with
        | Some event -> 
            Ok event
        | None ->
            eventTypes
            |> Seq.choose (fun eventType -> eventType |> Attribute.tryGet<EventTypeAttribute> |> Option.map (fun attribute -> eventType, attribute))
            |> Seq.tryFind (fun (_, attribute) -> attribute.EventType |> String.like name)
            |> Option.map fst
            |> Result.ofOption (UnknownEventType name)

    let addConsumer queue consumerTag =
        consumerManager.Post <| AddConsumer (queue, consumerTag)

    let tryGetConsumer queue =
        consumerManager.PostAndReply(fun reply -> GetConsumer (queue, reply))

    let removeConsumer queue = 
        consumerManager.Post <| RemoveConsumer queue

    let private expectedProperties = typeof<RabbitMqMessage>.GetProperties()

    let getEnvelopeAndEventData (serializer: IJsonSerializer) (memory: System.ReadOnlyMemory<byte>) =
        asyncResult {
            let body = memory.ToArray()
            let json = body |> Utf8.toString
            let! message = json |> serializer.DeserializeString<RabbitMqMessage> |> RabbitMqError.mapSerializationError            
            
            let envelope = JObject.Parse(json)     
            let eventData = JObject.Parse(message.Body)            
            
            if eventData.ContainsKey("eventId") |> not then                
                eventData.Add("eventId", JToken.FromObject(message.Id))
            
            let customProperties = 
                [for property in (envelope :> IEnumerable<KeyValuePair<string, JToken>>) do
                    if expectedProperties |> Seq.exists (fun p -> property.Key |> String.like p.Name) |> not then
                        yield (property.Key, property.Value)
                ]

            for (name, value) in customProperties do
                eventData.Add(name, value)

            return (message, eventData.ToString())
        }