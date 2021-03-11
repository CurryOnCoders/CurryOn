namespace CurryOn.DependencyInjection

/// Defines a standard 'Reader' monad
type Reader<'a, 'b> = Reader of ('a -> 'b) 

module Reader = 
    let run x (Reader f) = 
        f x

    let create x = 
        Reader (fun _ -> x) 
  
    let bind f x = 
        let future state =
            let z = run state x 
            run state (f z)
        Reader future

    let map f x =
        bind (f >> create) x

type ReaderBuilder<'t, 'u> () = 
    member __.Bind (x, f) = Reader.bind f x 
    member __.Return (x) = Reader.create x 
    member __.ReturnFrom x = x  
    member __.Zero () = Reader.create ()
    member __.Delay (f) = f()
    member __.Combine (a, b) =
        a |> Reader.bind (fun () -> b)
    member this.TryFinally(body, compensation) =
        try 
            this.ReturnFrom(body())
        finally 
            compensation()
    member this.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> Reader<'a, 'b>) : Reader<'a, 'b> = 
        let body' = fun () -> binder resource
        this.TryFinally(body', fun () -> 
            match resource with 
                | null -> () 
                | disp -> disp.Dispose())

    member this.While (guard, body: unit -> Reader<_,_>) =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))

[<AutoOpen>]
module ReaderMonad =
    let reader<'a, 'b> = ReaderBuilder<'a, 'b>() 