
#load "Awaitable.fs"


open CurryOn.Awaitable


let a () = async { return 5 }
let t () = System.Threading.Tasks.Task.Run(fun () -> 10)
let e = new Event<int>()

let test () =
    await {
        let! x = a()
        let! y = t()
        let! z = e.Publish

        return x + y + z
    }


async {
    let value = test() |> Awaitable.wait
    printfn "%d" value
} |> Async.Start

e.Trigger 20