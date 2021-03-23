namespace CurryOn.EventBus.RabbitMq

open CurryOn
open CurryOn.Domain
open CurryOn.EventBus
open System.Collections.Concurrent

type RabbitMqEventBus (connectionInfo: IRabbitMqConnection) =   
    let mapError = AsyncResult.mapError RabbitMqError.toEventBusError
    let getSubscriberId topic (subscriber: ISubscriber) = sprintf "%s.%s" (topic |> Topic.value) subscriber.PartyId
    let subscriberChannels = ConcurrentDictionary<string, RabbitMqChannel>()
    
    let addChannel key channel =
        subscriberChannels.AddOrUpdate(key, channel, fun _ _ -> channel) |> ignore

    let getChannel key =
        match subscriberChannels.TryGetValue key with
        | true, channel -> Some channel
        | _ -> None

    member __.PublishMany<'event when 'event :> IEvent> topic (events: 'event seq) =  
        asyncResult {
            use! channel = RabbitMqChannel.Connect<'event> connectionInfo topic |> AsyncResult.mapError List.singleton            

            return!
                events 
                |> Seq.map channel.Publish
                |> AsyncResult.Parallel
                |> AsyncResult.map ignore
        }

    member __.Publish<'event when 'event :> IEvent> topic event =          
        asyncResult {
            use! channel = RabbitMqChannel.Connect<'event> connectionInfo topic
            return! event |> channel.Publish
        }

    member __.Subscribe<'event when 'event :> IEvent> topic (subscriber: ISubscriber<'event>) =
        asyncResult {
            let! channel = RabbitMqChannel.Connect<'event> connectionInfo topic
            let! consumer = subscriber|> channel.Subscribe
            let subscriberId = subscriber |> getSubscriberId topic
            return addChannel subscriberId channel
        }

    member __.SubscribeAll topic (subscriber: IDynamicSubscriber) =
        asyncResult {
            let! channel = RabbitMqChannel.ConnectTopic connectionInfo topic
            let! consumer = subscriber|> channel.SubscribeAll
            let subscriberId = subscriber |> getSubscriberId topic
            return addChannel subscriberId channel
        }

    member __.Unsubscribe<'event when 'event :> IEvent> topic (subscriber: ISubscriber<'event>) =
        asyncResult {
            let subscriberId = subscriber |> getSubscriberId topic
            let channel = getChannel subscriberId
            match channel with
            | Some rabbit ->
                try do! subscriber |> rabbit.Unsubscribe
                finally rabbit.Channel.Dispose()
            | None ->
                return! Error <| SubscriberNotfound subscriber.PartyId
        }

    member __.UnsubscribeAll topic (subscriber: IDynamicSubscriber) =
        asyncResult {
            let subscriberId = subscriber |> getSubscriberId topic
            let channel = getChannel subscriberId
            match channel with
            | Some rabbit ->
                try do! subscriber |> rabbit.UnsubscribeAll
                finally rabbit.Channel.Dispose()
            | None ->
                return! Error <| SubscriberNotfound subscriber.PartyId
        }

    interface IEventBus with
        member this.Publish topic event =
            this.Publish topic event |> mapError
        member this.PublishMany topic events =
            this.PublishMany topic events
            |> AsyncResult.mapError (List.map RabbitMqError.toEventBusError)
        member this.Subscribe topic subscriber = 
            this.Subscribe topic subscriber |> mapError
        member this.SubscribeAll topic subscriber =
            this.SubscribeAll topic subscriber |> mapError
        member this.Unsubscribe topic subscriber = 
            this.Unsubscribe topic subscriber |> mapError
        member this.UnsubscribeAll topic subscriber =
            this.UnsubscribeAll topic subscriber |> mapError
