namespace CurryOn.Tracing

open OpenTracing
open System

type TracingError =
| OpenTracingNotConfigured
| InvalidTraceProviderType of System.Type
| UnexpectedTracingError of exn

[<CustomComparison; CustomEquality>]
type TraceLevel =
    | Verbose
    | Detailed
    | Normal
    | Minimal
    member private this.Compare(level) =        
        match this, level with
        | Verbose, Verbose -> 0
        | Verbose, _ -> 1
        | Detailed, Verbose -> -1
        | Detailed, Detailed -> 0
        | Detailed, _ -> 1
        | Normal, Verbose -> -1
        | Normal, Detailed -> -1
        | Normal, Normal -> 0
        | Normal, Minimal -> 1
        | Minimal, Minimal -> 0
        | Minimal, _ -> -1
    override this.Equals(o) =
        match o with
        | :? TraceLevel as t -> this.Compare(t) = 0
        | _ -> false
    override this.GetHashCode() =
        match this with
        | Verbose -> 4
        | Detailed -> 3
        | Normal -> 2
        | Minimal -> 1
    interface IComparable<TraceLevel> with
       member this.CompareTo(level) =
        this.Compare(level)
    interface IComparable with
        member this.CompareTo(o) =
            match o with
            | :? TraceLevel as t-> this.Compare(t)
            | _ -> -1
    interface IEquatable<TraceLevel> with
        member this.Equals(level) =
            this.Compare(level) = 0

type IDistributedTracing =
    abstract member Tracer: ITracer
    abstract member Enabled: bool
    abstract member Level: TraceLevel
