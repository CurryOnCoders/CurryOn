namespace CurryOn.EventBus.RabbitMq

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Domain
open CurryOn.EventBus
open CurryOn.Logging
open CurryOn.Serialization
open RabbitMQ.Client
open System
open System.Collections.Concurrent
open System.Threading.Tasks

type internal RabbitMqChannel =
    {
        Topic: string
        Auditing: IEventBusAuditing
        Serializer: IJsonSerializer
        RoutingKey: string
        Channel: IModel
        Properties: IBasicProperties
    } 
        interface IRabbitMqChannel with
            member this.Exchange = this.Topic
            member this.RoutingKey = this.RoutingKey
            member this.Dispose () =
                try this.Channel.Close()
                finally this.Channel.Dispose()

        static member private ConnectChannel connectionInfo topic routingKey = 
            asyncResult {
                let channelInfo = 
                    { new IRabbitMqChannel with
                        member __.Exchange = topic |> Topic.value
                        member __.RoutingKey = routingKey
                        member __.Dispose () = ()                            
                    }
              
                let! (channel, properties) = RabbitMq.connectChannel connectionInfo channelInfo
                let! container = Container.get() 
                let! provider = container |> Result.ofOption (DependencyInjectionFailure ServiceProviderNotRegistered)
                let! serializer = provider |> DependencyInjection.getService<IJsonSerializer> |> Result.mapError DependencyInjectionFailure

                return 
                    {
                        Topic = topic |> Topic.value
                        Auditing = connectionInfo.Auditing
                        Serializer = serializer
                        RoutingKey = routingKey
                        Channel = channel
                        Properties = properties
                    }
            }

        static member Connect<'event when 'event :> IEvent> connectionInfo topic =
            let routingKey = EventBus.getRoutingKey<'event>()
            RabbitMqChannel.ConnectChannel connectionInfo topic routingKey
              
        static member ConnectTopic connectionInfo topic =
            RabbitMqChannel.ConnectChannel connectionInfo topic "#"

        member rabbit.Audit direction routingKey (message: RabbitMqMessage) =
            asyncResult {
                try
                    if rabbit.Auditing.Enabled && rabbit.Topic |> String.notLike "*.Audit" then
                        let! messageBody = message |> rabbit.Serializer.SerializeToString |> Result.mapError SerializationError
                        let auditEvent =
                            {
                                EventId = message.Id
                                Topic = rabbit.Topic
                                RoutingKey = routingKey
                                Direction = direction
                                Message = messageBody
                                Client = Environment.MachineName
                                UserName = 
                                    if Environment.UserDomainName |> isNullOrEmpty 
                                    then Environment.UserName
                                    else sprintf "%s\\%s" Environment.UserDomainName Environment.UserName
                                Timestamp = DateTime.UtcNow
                                Error = null
                                Retries = 0
                            }
                        let message = auditEvent |> RabbitMqMessage.create rabbit.Serializer 
                        let! bytes = message |> rabbit.Serializer.Serialize |> Result.mapError SerializationError |> Result.map ReadOnlyMemory
                        rabbit.Channel.BasicPublish(rabbit.Auditing.Topic, "AuditEvent", rabbit.Properties, bytes)
                with ex ->                    
                    return! Error <| RabbitAuditingError ex.Message
            }
            

        member rabbit.Publish<'event when 'event :> IEvent> (event: 'event) =   
            asyncResult {
                try 
                    let routingKey = event |> Event.getRoutingKey
                    let! message = event |> RabbitMqMessage.create rabbit.Serializer   
                    do! message |> rabbit.Audit Publish routingKey
                    let! bytes = message |> rabbit.Serializer.Serialize |> Result.mapError SerializationError |> Result.map ReadOnlyMemory
                    return! Ok <| rabbit.Channel.BasicPublish(rabbit.Topic, routingKey, rabbit.Properties, bytes)
                with ex -> 
                    return! Error <| RabbitPublishingError (event, ex.Message)
            }

        member rabbit.Subscribe<'event when 'event :> IEvent> (subscriber: ISubscriber<'event>) =
            asyncResult {
                let queueName = sprintf "%s.Subscriber.%s.%s" rabbit.Topic subscriber.PartyId rabbit.RoutingKey
                try
                    let subscription = new RabbitMqSubscriber<'event>(rabbit, subscriber)
                    let queue = rabbit.Channel.QueueDeclare(queueName, durable = true, exclusive = subscriber.IsExclusive, autoDelete = false) 
                    rabbit.Channel.QueueBind(queue.QueueName, rabbit.Topic, rabbit.RoutingKey)
                    let consumerTag = rabbit.Channel.BasicConsume(queueName, false, subscription)
                    RabbitMq.addConsumer queueName consumerTag
                    return! Ok queueName
                with ex ->
                    return! Error <| RabbitChannelError ex.Message
            }

        member rabbit.SubscribeAll (subscriber: IDynamicSubscriber) =
            asyncResult {
                let queueName = sprintf "%s.Subscriber.%s" rabbit.Topic subscriber.PartyId
                try
                    let subscription = new RabbitMqDynamicSubscriber(rabbit, subscriber)
                    let queue = rabbit.Channel.QueueDeclare(queueName, durable = true, exclusive = subscriber.IsExclusive, autoDelete = false) 
                    rabbit.Channel.QueueBind(queue.QueueName, rabbit.Topic, "#") // '#' is a wildcard in Rabbit MQ, so this will bind to all routing keys
                    let consumerTag = rabbit.Channel.BasicConsume(queueName, false, subscription)
                    RabbitMq.addConsumer queueName consumerTag
                    return! Ok queueName
                with ex ->
                    return! Error <| RabbitChannelError ex.Message
            }

        member rabbit.Unsubscribe<'event when 'event :> IEvent> (subscriber: ISubscriber<'event>) =
            asyncResult {
                let queueName = sprintf "%s.Subscriber.%s.%s" rabbit.Topic subscriber.PartyId rabbit.RoutingKey
                let consumer = RabbitMq.tryGetConsumer queueName
                match consumer with
                | Some consumerTag ->
                    try
                        rabbit.Channel.BasicCancel(consumerTag)
                        rabbit.Channel.QueueDelete(queueName, ifUnused = false, ifEmpty = false) |> ignore                        
                        return! Ok <| RabbitMq.removeConsumer queueName
                    with ex ->
                        return! Error <| RabbitChannelError ex.Message
                | None ->
                    return! Ok ()
            }

        member rabbit.UnsubscribeAll (subscriber: IDynamicSubscriber) =
            asyncResult {
                let queueName = sprintf "%s.Subscriber.%s" rabbit.Topic subscriber.PartyId
                let consumer = RabbitMq.tryGetConsumer queueName
                match consumer with
                | Some consumerTag ->
                    try
                        rabbit.Channel.BasicCancel(consumerTag)
                        rabbit.Channel.QueueDelete(queueName, ifUnused = false, ifEmpty = false) |> ignore
                        return! Ok <| RabbitMq.removeConsumer queueName
                    with ex ->
                        return! Error <| RabbitChannelError ex.Message
                | None ->
                    return! Ok ()
            }

and [<AbstractClass>] internal RabbitMqSubscriberBase(rabbit: RabbitMqChannel, subscriber: ISubscriber) =
    inherit AsyncDefaultBasicConsumer(rabbit.Channel)    
    let retries = ConcurrentDictionary<Guid, int>()

    let errorQueue = 
        let queueName = sprintf "%s.Subscriber.%s.%s.Error" rabbit.Topic subscriber.PartyId rabbit.RoutingKey
        let queue = rabbit.Channel.QueueDeclare(queueName, durable = true, exclusive = false, autoDelete = false) 
        queue.QueueName

    let auditError routingKey (message: RabbitMqErrorMessage) =
        asyncResult {
            if rabbit.Auditing.Enabled && rabbit.Topic |> String.notLike "*.Audit" then
                let! payload = rabbit.Serializer.SerializeToString(message) |> RabbitMqError.mapSerializationError
                let auditEvent =
                    {
                        EventId = message.Id
                        Topic = rabbit.Topic
                        RoutingKey = routingKey
                        Direction = Subscribe subscriber.PartyId
                        Message = payload
                        Client = Environment.MachineName
                        UserName = 
                            if Environment.UserDomainName |> isNullOrEmpty 
                            then Environment.UserName
                            else sprintf "%s\\%s" Environment.UserDomainName Environment.UserName
                        Timestamp = DateTime.UtcNow
                        Error = message.ErrorMessage
                        Retries = message.RetryCount
                    }
                let message = auditEvent |> RabbitMqMessage.create rabbit.Serializer   
                let! json = rabbit.Serializer.Serialize(message) |> RabbitMqError.mapSerializationError |> Result.map ReadOnlyMemory
                rabbit.Channel.BasicPublish(rabbit.Auditing.Topic, "AuditEvent", rabbit.Properties, json)
        }

    let nack deliveryTag =
        asyncResult {
            rabbit.Channel.BasicNack(deliveryTag, false, true)
        }

    let reject deliveryTag =
        asyncResult {
            rabbit.Channel.BasicReject(deliveryTag, false)       
        }

    let poison deliveryTag error retries (message: RabbitMqMessage) =       
        asyncResult {
            let errorMessage = { Id = message.Id; EventType = message.EventType; ErrorMessage = error; Body = message.Body; RetryCount = retries; DatePublished = message.DatePublished; DateRejected = DateTime.UtcNow }
            let! bytes = rabbit.Serializer.Serialize(errorMessage) |> RabbitMqError.mapSerializationError |> Result.map ReadOnlyMemory
            rabbit.Channel.BasicPublish("", errorQueue, rabbit.Properties, bytes)
            do! errorMessage |> auditError message.EventType
            do! reject deliveryTag
        }

    let handleRetry deliveryTag error (message: RabbitMqMessage) =
        asyncResult {
            try 
                match subscriber.Retries with
                | NoRetries ->
                    do! message |> poison deliveryTag error 0
                | LimitedRetries limit ->
                    let retryCount = retries.AddOrUpdate(message.Id, (fun _ -> 0), (fun _ current -> current + 1))
                    if retryCount < limit then
                        do! nack deliveryTag
                    else 
                        do! message |> poison deliveryTag error retryCount
                | InfiniteRetries ->
                    do! nack deliveryTag  
            with ex ->
                do! message |> poison deliveryTag error -1
        }

    member __.ProcessEvent<'a> (f: 'a -> AsyncResult<unit, EventBusSubscriberError>, deliveryTag, message: RabbitMqMessage, event: 'a) =
        asyncResult {
            let! result = 
                try
                    event |> f |> AsyncResult.toAsync
                with ex ->
                    async { return Error <| UnexpectedSubscriberError ex }

            try
                match result with
                | Ok _ ->
                    rabbit.Channel.BasicAck(deliveryTag, false)
                | Error subscriberError ->
                    match subscriberError with
                    | NonRetryableSubscriberError error ->                        
                        do! message |> poison deliveryTag error 0
                    | RetryableSubscriberError error ->
                        do! message |> handleRetry deliveryTag error
                    | UnexpectedSubscriberError ex ->
                        do! message |> poison deliveryTag ex.Message 0            
            with ex ->
                return! Error <| RabbitChannelError ex.Message
        }

and internal RabbitMqSubscriber<'event when 'event :> IEvent> (rabbit: RabbitMqChannel, subscriber: ISubscriber<'event>) =
    inherit RabbitMqSubscriberBase(rabbit, subscriber)    

    override this.HandleBasicDeliver(consumerTag, deliveryTag, redelivered, exchange, routingKey, properties, body) =
        async {
            let! result =
                asyncResult {
                    if routingKey = rabbit.RoutingKey then
                        let! message, eventData = RabbitMq.getEnvelopeAndEventData rabbit.Serializer body
                        do! message |> rabbit.Audit (Subscribe subscriber.PartyId) routingKey
                        let! event = eventData |> rabbit.Serializer.DeserializeString<'event> |> RabbitMqError.mapSerializationError
                        do! this.ProcessEvent(subscriber.HandleEvent, deliveryTag, message, event)    
                    else
                        rabbit.Channel.BasicReject(deliveryTag, false)
                } |> AsyncResult.toAsync

            match result with
            | Ok _ -> ()
            | Error error ->
                rabbit.Channel.BasicReject(deliveryTag, false)
        } |> Async.StartAsTask :> Task

and internal RabbitMqDynamicSubscriber (rabbit: RabbitMqChannel, subscriber: IDynamicSubscriber) =
    inherit RabbitMqSubscriberBase(rabbit, subscriber)    

    override this.HandleBasicDeliver(consumerTag, deliveryTag, redelivered, exchange, routingKey, properties, body) =
        async {
            let! result =
                asyncResult { 
                    let! message, eventData = RabbitMq.getEnvelopeAndEventData rabbit.Serializer body
                    do! message |> rabbit.Audit (Subscribe subscriber.PartyId) routingKey
                    let! eventType = message.EventType |> RabbitMq.findEventType
                    let! event = eventData |> rabbit.Serializer.DeserializeStringAsType eventType |> Result.map unbox<IEvent> |> RabbitMqError.mapSerializationError
                    do! this.ProcessEvent(subscriber.HandleEvent, deliveryTag, message, event)
                } |> AsyncResult.toAsync

            match result with
            | Ok _ -> ()
            | Error error ->
                rabbit.Channel.BasicReject(deliveryTag, false)
        } |> Async.StartAsTask :> Task