namespace CurryOn.Tasks.Test

open CurryOn.Tasks
open CurryOn.UnitTest
open Microsoft.VisualStudio.TestTools.UnitTesting

type TestError =
| DivideByZero
| UnexpectedError of exn

[<TestClass>]
type TaskResultTests () =
    let client = new System.Net.Http.HttpClient()

    // Returns synchronously
    let square (x) =
        x * x

    // Returns a Task
    let fetch (url: string) =
        client.GetStringAsync(url)

    // Returns an Async
    let read (path) =
        async {
            let file = System.IO.FileInfo path
            use stream = file.OpenRead()
            let! bytes = stream.AsyncRead(int file.Length)
            return System.Text.Encoding.UTF8.GetString(bytes)
        }

    // Returns a Result
    let divideBy y x =
        if y = 0 then
            Error DivideByZero
        else
            Ok (x / y)

    [<TestMethod>]
    member __.``TaskResult can be synchronous`` () =
        let result : Result<int, TestError> = 
            taskResult {
                let y = square 2
                return square y
            } |> TaskResult.toResult
        match result with
        | Ok value ->
            value |> equals 16
        | Error e ->
            fail (sprintf "%A" e)

    [<TestMethod>]
    member __.``TaskResult can await Tasks`` () =
        let result : Result<int, TestError> = 
            taskResult {
                let! page = fetch "http://www.google.com"
                return page.Length
            } |> TaskResult.toResult
        match result with
        | Ok length ->
            length |> greaterThan 100
        | Error e ->
            fail (sprintf "%A" e)

    [<TestMethod>]
    member __.``TaskResult can await Asyncs`` () =
        let result : Result<int, TestError> = 
            taskResult {
                let! data = read "sample.txt"
                return data.Length
            } |> TaskResult.toResult
        match result with
        | Ok length ->
            length |> equals 29
        | Error e ->
            fail (sprintf "%A" e)

    [<TestMethod>]
    member __.``TaskResult can await Results`` () =
        let result : Result<int, TestError> = 
            taskResult {
                let! answer = 8 |> divideBy 2
                return answer |> square
            } |> TaskResult.toResult
        match result with
        | Ok value ->
            value |> equals 16
        | Error e ->
            fail (sprintf "%A" e)