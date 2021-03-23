namespace CurryOn.Tracing

open CurryOn.DependencyInjection

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tracing =
    let inline trace level name f = 
        injected {
            let! tracing = inject<IDistributedTracing>()
            if tracing.Level >= level then
                use trace = tracing.Tracer |> TraceEvent.create name |> TraceEvent.start
                try 
                    return f ()
                finally
                    trace |> TraceEvent.finish
            else
                return f ()
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Trace =
    open System.Reflection

    let inline traceCurrent level f =
        let currentMethod = MethodBase.GetCurrentMethod()
        let name = sprintf "%s.%s" currentMethod.ReflectedType.Name currentMethod.Name
        Tracing.trace level name f

    let inline verbose f =  traceCurrent Verbose f
    let inline detailed f = traceCurrent Detailed f
    let inline normal f = traceCurrent Normal f
    let inline minimal f = traceCurrent Minimal f