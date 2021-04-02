namespace CurryOn.IO

open FSharp.Control
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module File =
    let exists (filePath: string) =
        File.Exists(filePath)

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

    let openBytes (filePath: string) =
        new BinaryReader(new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))

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