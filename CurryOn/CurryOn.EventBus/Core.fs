namespace CurryOn.EventBus

open CurryOn
open CurryOn.Domain
open System

type EventBusSubscriberError =
| RetryableSubscriberError of string
| NonRetryableSubscriberError of string
| UnexpectedSubscriberError of exn

type EventBusError =
| UnsupportedEventType of string
| ConnectionError of string
| SubscriberError of EventBusSubscriberError
| TransportError of string
| ErrorPublishingEvent of IEvent * string
| MessageFormatError of string
| NoEventBusConfigured
| UnhandledErrorProcessingEvent of exn

type IParty =
    abstract member PartyId: string

type IPublisher = inherit IParty

type RetrySetting =
| NoRetries
| LimitedRetries of int
| InfiniteRetries

type ConsumerMode =
| Exclusive
| CompetingConsumers

type ISubscriber = 
    inherit IParty
    abstract member Mode: ConsumerMode
    abstract member Retries: RetrySetting

type ISubscriber<'event when 'event :> IEvent> =
    inherit ISubscriber
    abstract member HandleEvent: 'event -> AsyncResult<unit, EventBusSubscriberError>

type IDynamicSubscriber =
    inherit ISubscriber
    abstract member HandleEvent: IEvent -> AsyncResult<unit, EventBusSubscriberError>

[<Struct>] type Topic = private Topic of string

module Topic =
    let create (topic: string) =
        if topic |> String.like "EventBus.*"
        then topic
        else sprintf "EventBus.%s" topic 
        |> Topic

    let name (Topic topic) = 
        topic |> String.replace "EventBus." ""

    let value (Topic topic) = topic

type IEventBus =
    abstract member Publish<'event when 'event :> IEvent> : Topic -> 'event -> AsyncResult<unit, EventBusError>
    abstract member PublishMany<'event when 'event :> IEvent> : Topic -> 'event seq -> AsyncResult<unit, EventBusError list>
    
    abstract member Subscribe<'event when 'event :> IEvent> : Topic -> ISubscriber<'event> -> AsyncResult<unit, EventBusError>
    abstract member SubscribeAll : Topic -> IDynamicSubscriber -> AsyncResult<unit, EventBusError>
    
    abstract member Unsubscribe<'event when 'event :> IEvent> : Topic -> ISubscriber<'event> -> AsyncResult<unit, EventBusError>
    abstract member UnsubscribeAll : Topic -> IDynamicSubscriber -> AsyncResult<unit, EventBusError>

type IEventBusAuditing =
    abstract member Enabled: bool
    abstract member Topic: string

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct ||| AttributeTargets.Interface ||| AttributeTargets.Enum, AllowMultiple = false)>]
type RoutingKeyAttribute (routingKey: string) =
    inherit Attribute()
    member __.RoutingKey = routingKey

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct ||| AttributeTargets.Interface ||| AttributeTargets.Enum, AllowMultiple = false)>]
type EventTypeAttribute (eventType: string) =
    inherit Attribute()
    member __.EventType = eventType

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Module, AllowMultiple = true)>]
type SubscriptionsAttribute(partyId: string) =
    inherit Attribute ()
    member __.PartyId = partyId

[<AbstractClass>]
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type SubscriberAttributeBase(topic: string, mode: ConsumerMode, retries: RetrySetting) =
    inherit Attribute ()
    let eventBusTopic = topic |> Topic.create
    member __.Topic = eventBusTopic
    member __.Retries = retries
    member __.Mode = mode

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private SubscriberAttritue =
    let DefaultMode = CompetingConsumers
    let DefaultRetries = NoRetries

    let getRetry retryCount =
        match retryCount with
        | 0 -> NoRetries
        | n when n > 0 && n < Int32.MaxValue -> LimitedRetries n
        | _ -> InfiniteRetries
    let getMode exclusive =
        if exclusive then Exclusive else CompetingConsumers

type SubscriberAttribute(topic, mode: ConsumerMode, retries: RetrySetting) =
    inherit SubscriberAttributeBase(topic, mode, retries)    
    new (topic: string) = SubscriberAttribute(topic, DefaultMode, DefaultRetries)
    new (topic: string, retryCount: int) = SubscriberAttribute(topic, DefaultMode, getRetry retryCount)
    new (topic: string, exclusive: bool) = SubscriberAttribute(topic, getMode exclusive, DefaultRetries)
    new (topic: string, retryCount, exclusive) = SubscriberAttribute(topic, getMode exclusive, getRetry retryCount)


type DynamicSubscriberAttribute(topic, mode: ConsumerMode, retries: RetrySetting) =
    inherit SubscriberAttributeBase(topic, mode, retries)    
    new (topic: string) = DynamicSubscriberAttribute(topic, DefaultMode, DefaultRetries)
    new (topic: string, retryCount: int) = DynamicSubscriberAttribute(topic, DefaultMode, getRetry retryCount)
    new (topic: string, exclusive: bool) = DynamicSubscriberAttribute(topic, getMode exclusive, DefaultRetries)
    new (topic: string, retryCount, exclusive) = DynamicSubscriberAttribute(topic, getMode exclusive, getRetry retryCount)


type AuditEventDirection =
| Publish
| Subscribe of string

[<CLIMutable>]
type AuditEvent =
    {
        EventId: Guid
        Topic: string
        RoutingKey: string
        Direction: AuditEventDirection
        Client: string
        UserName: string
        Message: string
        Timestamp: DateTime
        Error: string
        Retries: int
    } interface IEvent with
        member this.EventId = this.EventId