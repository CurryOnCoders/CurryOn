namespace CurryOn.Tracing

open CurryOn.DependencyInjection
open OpenTracing
open System

module TraceEvent =
    let create name (tracer: ITracer) =
        tracer.BuildSpan(name)

    let asNewRoot (event: ISpanBuilder) =
        event.IgnoreActiveSpan()

    let reference name context (event: ISpanBuilder) =
        event.AddReference(name, context)

    let withParent (parent: ISpan) (event: ISpanBuilder) =
        event.AsChildOf(parent)

    let withParentContext (parentContext: ISpanContext) (event: ISpanBuilder) =
        event.AsChildOf(parentContext)

    let tag (key: string) value (event: ISpanBuilder) =
        event.WithTag(key, sprintf "%O" value)

    let setTimestamp timestamp (event: ISpanBuilder) =
        event.WithStartTimestamp(timestamp)

    let setStartTime (dateTime: DateTime) (event: ISpanBuilder) =
        event.WithStartTimestamp(DateTimeOffset.op_Implicit dateTime)

    let start (event: ISpanBuilder) =
        event.StartActive()

    let log (message: string) (scope: IScope) =
        scope.Span.Log(message) |> ignore
        scope

    let finish (scope: IScope) =
        scope.Span.Finish()

    let finishedAt timestamp (scope: IScope) =
        scope.Span.Finish(timestamp)

module Tracer =
    let get () =
        injected {
            let! tracing = inject<IDistributedTracing>()
            return tracing.Tracer
        }

    let trace f (event: ISpanBuilder) =        
        use scope = event |> TraceEvent.start
        try
            f scope
        finally
            scope |> TraceEvent.finish

