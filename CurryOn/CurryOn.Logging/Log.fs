namespace CurryOn.Logging

open CurryOn.DependencyInjection
open Microsoft.Extensions.Logging

/// Functional logging module, based on Printf syntax
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Log =
    type LogMessage =
        | Trace of string
        | Debug of string
        | Info of string
        | Warning of (string * exn option)
        | Error of (string * exn option)
        | Critical of (string * exn)
        member this.Message =
            match this with
            | Trace message -> message
            | Debug message -> message
            | Info message -> message
            | Warning (message, _) -> message
            | Error (message, _) -> message
            | Critical (message, _) -> message
    
    let inline log message =
        injected {
            let! logger = inject<ILogger>()
            match message with
            | Trace message ->
                logger.LogTrace(message)
            | Debug message ->
                logger.LogDebug(message)
            | Info message ->
                logger.LogDebug(message)
            | Warning (message, ex) ->
                match ex with
                | Some ex ->
                    logger.LogWarning(ex, message)
                | None ->
                    logger.LogWarning(message)
            | Error (message, ex) ->
                match ex with 
                | Some ex ->
                    logger.LogError(ex, message)
                | None ->
                    logger.LogError(message)
            | Critical (message, ex) ->
                logger.LogCritical(ex, message)
        }

    let inline logr message =
        injected {
            do! log message
            return message.Message
        }

    let inline debug message = 
        log (Debug message)

    let inline debugf format = 
        Printf.kprintf (Debug >> log) format
    
    let inline debugr message = 
        logr (Debug message)

    let inline debugfr format = 
        Printf.kprintf (Debug >> logr) format
    
    let inline info message = 
        log (Info message)

    let inline infof format = 
        Printf.kprintf (Info >> log) format
    
    let inline infor message = 
        logr (Info message)

    let inline infofr format = 
        Printf.kprintf (Info >> logr) format

    let inline warn message = 
        log (Warning (message, None))

    let inline warnf format = 
        Printf.kprintf ((fun msg -> Warning (msg, None)) >> log) format
    
    let inline warnx ex message = 
        log (Warning (message, Some ex))
    
    let inline warnxf ex format = 
        Printf.kprintf ((fun msg -> Warning (msg, Some ex)) >> log) format

    let inline warnr message = 
        logr (Warning (message, None))

    let inline warnfr format = 
        Printf.kprintf ((fun msg -> Warning (msg, None)) >> logr) format

    let inline warnxr ex message = 
        logr (Warning (message, Some ex))

    let inline warnxfr ex format  = 
        Printf.kprintf ((fun msg -> Warning (msg, Some ex)) >> logr) format     
        
    let inline error message = 
        log (Error (message, None))

    let inline errorf format = 
        Printf.kprintf ((fun msg -> Error (msg, None)) >> log) format
    
    let inline errorx ex message = 
        log (Error (message, Some ex))
    
    let inline errorxf ex format = 
        Printf.kprintf ((fun msg -> Error (msg, Some ex)) >> log) format

    let inline errorr message = 
        logr (Error (message, None))

    let inline errorfr format = 
        Printf.kprintf ((fun msg -> Error (msg, None)) >> logr) format

    let inline errorxr ex message = 
        logr (Error (message, Some ex))

    let inline errorxfr ex format  = 
        Printf.kprintf ((fun msg -> Error (msg, Some ex)) >> logr) format
        
    let inline critial message = 
        log (Critical (message, exn message))

    let inline critialf format = 
        Printf.kprintf ((fun msg -> Critical (msg, exn msg)) >> log) format
    
    let inline critialx ex message = 
        log (Critical (message, ex))
    
    let inline critialxf ex format = 
        Printf.kprintf ((fun msg -> Critical (msg, ex)) >> log) format

    let inline critialr message = 
        logr (Critical (message, exn message))

    let inline critialfr format = 
        Printf.kprintf ((fun msg -> Critical (msg, exn msg)) >> logr) format

    let inline critialxr ex message = 
        logr (Critical (message, ex))

    let inline critialxfr ex format  = 
        Printf.kprintf ((fun msg -> Critical (msg, ex)) >> logr) format