namespace CurryOn.EventBus.RabbitMq

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Domain
open CurryOn.EventBus
open CurryOn.Serialization
open System

type IRabbitMqConnection =
    abstract member Auditing: IEventBusAuditing
    abstract member UserName: string
    abstract member Password: string
    abstract member Servers: string []

type IRabbitMqChannel =
    inherit IDisposable
    abstract member Exchange: string
    abstract member RoutingKey: string

type RabbitMqError =
| DependencyInjectionFailure of DependencyInjectionError
| SerializationError of SerializationError
| RabbitMqNotConnected
| RabbitConnectionError of string * IRabbitMqConnection
| RabbitChannelError of string
| RabbitPublishingError of IEvent * string
| RabbitAuditingError of string
| RabbitSubscriberError of string
| UnknownEventType of string
| SubscriberNotfound of string
| UnhandledRabbitError of exn

type IRabbitMqMessage =
    abstract member Id: Guid
    abstract member EventType: string
    abstract member Body: string
    abstract member DatePublished: DateTime

[<CLIMutable>]
type RabbitMqMessage =
    {
        Id: Guid
        EventType: string
        Body: string
        DatePublished: DateTime
    } interface IEvent with 
        member this.EventId = this.Id
      interface IRabbitMqMessage with
        member this.Id = this.Id
        member this.EventType = this.EventType
        member this.Body = this.Body
        member this.DatePublished = this.DatePublished

[<CLIMutable>]
type RabbitMqErrorMessage =
    {
        Id: Guid
        EventType: string
        ErrorMessage: string
        RetryCount: int
        Body: string
        DatePublished: DateTime
        DateRejected: DateTime
    } interface IEvent with 
        member this.EventId = this.Id
      interface IRabbitMqMessage with
        member this.Id = this.Id
        member this.EventType = this.EventType
        member this.Body = this.Body
        member this.DatePublished = this.DatePublished

module RabbitMqMessage =
    let create<'event when 'event :> IEvent> (serializer: IJsonSerializer) (event: 'event) =
        result {
            let! textBody = event |> serializer.SerializeToString |> Result.mapError SerializationError
            return
                {
                    Id = event.EventId
                    EventType = event.GetType().Name
                    Body = textBody
                    DatePublished = DateTime.UtcNow
                }
        }        

[<AutoOpen>]
module internal Extensions =
    type ISubscriber with
        member this.IsExclusive =
            match this.Mode with
            | Exclusive -> true
            | _ -> false

module RabbitMqError =
    let toEventBusError = function
    | RabbitMqNotConnected -> 
        ConnectionError "Rabbit MQ Not Connected"
    | RabbitConnectionError (error, connection) ->
        let message = sprintf "Error Connecting to RabbitMQ on %A: %s" connection.Servers error
        ConnectionError message
    | RabbitChannelError error ->
        TransportError error
    | RabbitPublishingError (event, error) ->
        ErrorPublishingEvent (event, error)
    | RabbitAuditingError error ->
        TransportError <| sprintf "Error auditing event: %s" error
    | SerializationError error ->
        MessageFormatError <| sprintf "Serialization Error: %A" error
    | RabbitSubscriberError error ->
        TransportError <| sprintf "Subscriber Error: %s" error
    | UnknownEventType eventType ->
        UnsupportedEventType eventType
    | SubscriberNotfound partyId ->
        TransportError <| sprintf "Subscriber Not Found: %s" partyId
    | DependencyInjectionFailure _ ->
        NoEventBusConfigured
    | UnhandledRabbitError ex ->
        let message = sprintf "Unexpected RabbitMQ Error: %s \r\n %O" ex.Message ex
        TransportError message 

    
    let mapSerializationError<'event> (result: Result<'event, SerializationError>) =
        result |> Result.mapError (fun (error: SerializationError) -> 
            RabbitSubscriberError (sprintf "Error Deserializaing Event to Type '%s': %A" typeof<'event>.Name error))