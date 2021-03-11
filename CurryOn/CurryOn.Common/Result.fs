namespace CurryOn

open FSharp.Reflection
open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
    /// Apply a lifted function over a Result<'a, 'b>
    let apply wrappedFunction result = 
        match wrappedFunction, result with
        | Ok a, Ok b -> Ok (a b)
        | Error e, Ok s -> Error e
        | Ok s, Error e -> Error e
        | Error e, Error _ -> Error e

    /// Lift a function f into a Result and apply it to the given Result r
    let lift f r = apply (Ok f) r

    /// Execute either the success function or the failure function based on the result
    let either fSuccess fFailure = function
    | Ok value -> fSuccess value
    | Error error -> fFailure error

    /// Convert an Option<'a,'b> to a Result<'a,'b> where Some -> Ok and None -> error()
    let ofOption error = function
    | Some value -> Ok value
    | None -> Error error

    let inline private convert<'a,'b,'c> convertOk convertError: Result<'a,'b> -> 'c = function
        | Ok result -> convertOk result
        | Error error -> convertError error

    /// Convert Result<'a,'b> to Option<'a>
    let toOption<'a,'b> = convert<'a,'b,'a option> Some (ret None)
        
    /// Convert Result<'a,'b> to Choice<'a,'b>
    let toChoice<'a,'b> = convert<'a,'b,Choice<'a,'b>> Choice1Of2 Choice2Of2

    /// Convert Result<_,_> to bool where Ok -> true
    let toBool<'a,'b> = convert<'a,'b,bool> (ret true) (ret false)

    /// Check if the given Result is the Ok case
    let inline isOk r = toBool r
         
    /// Check if the given Result is the Error case
    let isError r = convert (ret false) (ret true) r

    let join results =
        results |> Seq.fold (fun acc cur ->
            match acc with
            | Ok values -> 
                match cur with
                | Ok value -> Ok (values @ [value])
                | Error error -> Error [error]
            | Error errors ->
                match cur with
                | Error error -> Error (error :: errors)
                | _ -> Error errors) (Ok [])

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Either =
    let inline ok x = Ok x

    let inline zero () = Ok ()

    let convertExceptionToResult<'result,'event> (ex: exn) =
        let union = FSharpType.GetUnionCases(typeof<Result<'result, 'event>>)
        if typeof<'event>.IsAssignableFrom(typeof<exn>) then 
            FSharpValue.MakeUnion(union.[1], [|ex |> box|]) |> unbox<Result<'result, 'event>>
        elif FSharpType.IsUnion typeof<'event> then 
            let cases = FSharpType.GetUnionCases(typeof<'event>)
            match cases |> Seq.tryFind (fun case -> case.GetFields().Length = 1 && case.GetFields().[0].PropertyType.IsAssignableFrom(typeof<exn>)) with
            | Some case -> 
               FSharpValue.MakeUnion(union.[1], [|FSharpValue.MakeUnion(case, [|ex |> box|]) |> unbox<'event> |> box|]) |> unbox<Result<'result, 'event>>
            | None -> 
               failwithf "No Union Case of Event Type %s Supports Construction from an Unhandled Exception: \r\n%O" typeof<'event>.Name ex
        else 
            failwithf "Unable To Construct a Failure of type %s from Unhandled Exception: \r\n%O" typeof<'event>.Name ex

    let inline delay<'result,'event> (f: unit -> Result<'result, 'event>) = fun () ->
        try f()
        with | ex -> convertExceptionToResult ex
                
    let inline run<'result,'event> (f: unit -> Result<'result, 'event>) =
        try f()
        with | ex -> convertExceptionToResult ex

    let inline (>>=) result f = Result.bind f result

    let inline (<*>) wrappedFunction result = Result.apply wrappedFunction result

    let inline (<!>) f result = Result.lift f result

type ResultBuilder() = 
    member __.Zero() = Either.zero()
    member __.Bind(x, f) = Result.bind f x
    member __.Return(x) = Either.ok x
    member __.ReturnFrom(x) = x
    member __.Yield(x) = Either.ok x
    member __.YieldFrom(x) = x
    member __.Combine (a, b) = Result.bind b a
    member __.Delay f = Either.delay f
    member __.Run f = Either.run f
    member __.TryWith (body, handler) =
        try body()
        with | ex -> handler ex
    member __.TryFinally (body, compensation) =
        try body()
        finally compensation()
    member this.Using(d:#IDisposable, body) =
        let result = fun () -> body d
        this.TryFinally(result, fun () -> 
            if d |> isNotNull
            then d.Dispose())
    member this.While (guard, body) =
        if guard () 
        then Result.bind (fun () -> this.While(guard, body)) (body())
        else this.Zero()
    member this.For(s:seq<_>, body) =
        this.Using(s.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext,
                this.Delay(fun () -> body enum.Current)))

[<AutoOpen>]
module EitherMonad =
    let result = ResultBuilder()
