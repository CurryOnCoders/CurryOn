namespace CurryOn.RabbitMq

open RabbitMQ.Client
open System
open System.Threading.Tasks

type RabbitMqContext =
    {
        Connection: IConnection
        Channel: IModel
        Properties: IBasicProperties
    } member this.Dispose () =
        try            
            this.Channel.Dispose()
            this.Connection.Dispose()
        with _ ->
            ()    
    interface IDisposable with
        member this.Dispose () = 
            this.Dispose()

type internal RabbitMqState =
    {
        ConnectionFactory: IConnectionFactory
        Context: RabbitMqContext option
    }

type RabbitMqError =
    | RabbitMqNotConnected
    | RabbitMqConnectionError of string
    | RabbitMqChannelError of string
    | UnexpectedRabbitMqError of exn
    member error.Message =
        match error with
        | RabbitMqNotConnected -> "Rabbit MQ is not connected"
        | RabbitMqConnectionError e -> sprintf "Connection Error: %s" e
        | RabbitMqChannelError e -> sprintf "Channel Error: %s" e
        | UnexpectedRabbitMqError e -> e.Message


type RabbitMqException (error: RabbitMqError) =
    inherit Exception(error.Message)
    member __.Error = error

type IRabbitMqChannelManager =
    abstract member ConnectionFactory: IConnectionFactory
    abstract member GetContext: unit -> Task<RabbitMqContext>
    abstract member GetConnection: unit -> Task<IConnection>
    abstract member GetChannel: unit -> Task<IModel>
    abstract member GetNewChannel: unit -> Task<IModel>