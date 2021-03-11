namespace CurryOn.Validation

open System

type ValidatedBuilder() = 
    member __.Zero() = ValidatedResult.zero()
    member __.Bind(x, f) = ValidatedResult.bind f x
    member __.Return(x) = ValidatedResult.success x
    member __.ReturnFrom(x) = x
    member __.Yield(x) = ValidatedResult.success x
    member __.YieldFrom(x) = x
    member __.Combine (a, b) = ValidatedResult.combine a b
    member __.Delay f = ValidatedResult.delay f
    member __.Run f = ValidatedResult.run f
    member __.TryWith (body, handler) =
        try body()
        with | ex -> handler ex
    member __.TryFinally (body, compensation) =
        try body()
        finally compensation()
    member this.Using(d:#IDisposable, body) =
        let result = fun () -> body d
        this.TryFinally (result, fun () ->
            match d with
            | null -> ()
            | d -> d.Dispose())
    member this.While (guard, body) =
        if guard () 
        then ValidatedResult.bind (fun () -> this.While(guard, body)) (body())
        else this.Zero()
    member this.For(s:seq<_>, body) =
        this.Using(s.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext,
                this.Delay(fun () -> body enum.Current)))


[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Validated =
    let validated = ValidatedBuilder()


