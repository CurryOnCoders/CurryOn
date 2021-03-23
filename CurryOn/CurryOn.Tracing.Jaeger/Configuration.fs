namespace CurryOn.Tracing.Jaeger

open CurryOn.Tracing
open System

type Sampling =
| Off
| All
| Probabilistic of float
| RateLimiting of float
| Remote of (TimeSpan * string)


type IJaegerTracingConfiguration =
    abstract member Enabled: bool
    abstract member Level: TraceLevel
    abstract member ServiceName: string
    abstract member AgentHost: string
    abstract member AgentPort: int
    abstract member SamplingMode: Sampling