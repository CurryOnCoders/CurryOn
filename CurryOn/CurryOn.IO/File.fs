namespace CurryOn.IO

open CurryOn
open FSharp.Control
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module File =
    let exists (filePath: string) =
        File.Exists(filePath)

    let name (filePath: string) =
        let fileInfo = FileInfo filePath
        let segments = fileInfo.Name.Split('.')

        match segments with
        | [| name |] ->
            name
        | segments ->
            segments
            |> Seq.take (segments.Length - 1)
            |> String.join "."

    let openText (filePath: string) =
        new StreamReader(filePath)

    let readAllText (filePath: string) =
        async {
            use reader = filePath |> openText
            return! reader.ReadToEndAsync() |> Async.AwaitTask
        }

    let readLines (filePath: string) =
        asyncSeq {
            use reader = filePath |> openText
            while not reader.EndOfStream do
                let! line = reader.ReadLineAsync() |> Async.AwaitTask
                yield line
        }   

    let readAllLines (filePath: string) =
        filePath |> readLines |> AsyncSeq.toArrayAsync

    let openStream mode (filePath: string) =
        match mode with
        | ReadOnly ->
            new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        | WriteOnly ->
            new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read)
        | ReadAndWrite ->
            new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
        | Exclusive ->
            new FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

    let openBytes (filePath: string) =
        new BinaryReader(filePath |> openStream ReadOnly)

    let readBytes (filePath: string) =
        asyncSeq {
            use reader = filePath |> openBytes            
            let buffer = Array.zeroCreate<byte> 8192

            let rec readBlock () =
                asyncSeq {                    
                    let bytesRead = reader.Read(buffer, 0, buffer.Length)
                    if bytesRead > 0 then
                        for byte in buffer do
                            yield byte
                        yield! readBlock()
                }

            yield! readBlock()
        }

    let readBlock blockSize (filePath: string) =                  
        use reader = filePath |> openBytes            
        let buffer = Array.zeroCreate<byte> blockSize
        let bytesRead = reader.Read(buffer, 0, buffer.Length)
        buffer |> Array.take bytesRead

    let readAllBytes (filePath: string) =
        async {
            let bytes = filePath |> readBytes
            return! bytes |> AsyncSeq.toArrayAsync
        }

    let openWrite (filePath: string) =
        new StreamWriter(filePath)

    let writeText (content: string) (filePath: string) =
        async {
            use writer = filePath |> openWrite
            do! writer.WriteAsync(content) |> Async.AwaitTask
            do! writer.FlushAsync() |> Async.AwaitTask
        }

    let writeBytes (content: byte []) (filePath: string) =
        async {
            use writer = new BinaryWriter(filePath |> openStream WriteOnly)
            writer.Write(content)
            writer.Flush()
        }