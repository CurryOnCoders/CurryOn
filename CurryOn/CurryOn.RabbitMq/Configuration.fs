namespace CurryOn.RabbitMq

type IRabbitMqConfiguration =
    abstract member Hosts: string []
    abstract member Port: int
    abstract member UserName: string
    abstract member Password: string
    abstract member PrefetchCount: int option
    abstract member VirtualHost: string