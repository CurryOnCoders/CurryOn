namespace CurryOn.EventBus

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Domain
open System

module EventBus =
    let private useBus<'result> (f: IEventBus -> AsyncResult<'result, EventBusError>) =
        injected {
            let! eventBus = inject<IEventBus>()
            return f eventBus
        }

    let getRoutingKeyByEventType (eventType: Type) =
        match eventType |> Attribute.tryGet<RoutingKeyAttribute> with
        | Some attribute -> attribute.RoutingKey
        | None -> eventType.Name
    
    let getRoutingKey<'event when 'event :> IEvent> () =
        typeof<'event> |> getRoutingKeyByEventType

    let publish<'event when 'event :> IEvent> topic (event: 'event) =
        useBus <| fun eventBus -> event |> eventBus.Publish topic

    let subscribeAll topic subscriber =
        useBus <| fun eventBus -> eventBus.SubscribeAll topic subscriber

    let subscribe<'event when 'event :> IEvent> topic subscriber =
        useBus <| fun eventBus -> eventBus.Subscribe<'event> topic subscriber

module Event =
    let getRoutingKey<'event when 'event :> IEvent> (event: 'event) =
        event.GetType() |> EventBus.getRoutingKeyByEventType 
