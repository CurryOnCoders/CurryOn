namespace CurryOn.DependencyInjection

open CurryOn
open System

type DependencyInjectionError =
| NoServiceFound of Type
| UnexpectedDependencyInjectionError of exn

/// Defines a specialized Reader monad for Dependency Injection
type Injected<'t, 'e> = Reader<IServiceProvider, Result<'t, 'e>>

module Injected = 
    let run x f = 
        Reader.run x f

    let create x = 
        Reader.create (Ok x)
  
    let bind<'a, 'b, 'e> (f: 'a -> Injected<'b, 'e>) (x: Injected<'a, 'e>) : Injected<'b, 'e> = 
        let future state =
            let result = run state x 
            match result with
            | Ok z ->
                run state (f z)
            | Error e ->
                Error e
        Reader future

    let bindResult<'a, 'b, 'e> (f: 'a -> Injected<'b, 'e>) (x: Result<'a, 'e>) : Injected<'b, 'e> =
        match x with
        | Ok z -> f z
        | Error e -> Reader (fun _ -> Error e)

    let map f x =
        bind (f >> create) x

    let mapError f (x: Injected<_,_>) : Injected<_,_> =
        let (Reader getResult) = x
        fun provider ->
            let result = getResult provider
            match result with
            | Ok value -> Ok value
            | Error e -> Error (f e)
        |> Reader 

    let ofResult (result: Result<_,_>) : Injected<_,_> =
        Reader.create result

    let join (elements: Injected<'a,'e> seq) : Injected<'a list, 'e> =
        elements |> Seq.fold (fun acc cur ->
            fun provider ->
                let result = run provider acc
                match result with
                | Ok values -> 
                    let next = run provider cur
                    match next with
                    | Ok value -> Ok (values @ [value])
                    | Error error -> Error error
                | Error error ->
                    Error error
            |> Reader) (create [])

    let ignore (i: Injected<_,_>) =
        i |> map ignore

    let orElseWith<'a, 'e> (f: unit -> 'a) (x: Injected<'a, 'e>) : Injected<'a, 'e> =
        let future state =
            let result = run state x 
            match result with
            | Ok z ->
                z
            | Error _ ->
                f ()
            |> Ok
        Reader future

type InjectionBuilder<'t> () =
    member __.Bind (x, f) : Injected<_,_> = Injected.bind f x
    member __.Bind (x, f) : Injected<_,_> = Injected.bindResult f x
    member __.Return (x) : Injected<_,_> = Injected.create x 
    member __.ReturnFrom (x: Injected<_,_>) = x    
    member __.ReturnFrom (x: Result<_,_>) = Injected.ofResult x
    member __.Zero () : Injected<_,_> = Injected.create ()
    member __.Delay (f) : Injected<_,_> = f()
    member __.Combine (a, b) : Injected<_,_> =
        a |> Injected.bind (fun () -> b)
    member this.TryWith(body : Injected<'a, 'b>, catchHandler : exn -> Injected<'a, 'b>) : Injected<'a, 'b> = 
        try
            this.ReturnFrom(body)
        with ex ->
            catchHandler ex
    member this.TryFinally(body: Injected<_,_>, compensation) : Injected<_,_> =
        try 
            this.ReturnFrom(body)
        finally 
            compensation()
    member this.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> Injected<'a, 'e>) : Injected<'a, 'e> = 
        let body = binder resource
        this.TryFinally(body, fun () -> 
            if resource |> isNotNull
            then resource.Dispose())

    member this.While (guard, body: unit -> Injected<_,_>) : Injected<_,_> =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) : Injected<_,_> =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))
