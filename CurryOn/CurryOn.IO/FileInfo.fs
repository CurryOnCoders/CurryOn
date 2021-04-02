namespace CurryOn.IO

open FSharp.Control
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FileInfo =
    let exists (fileInfo: FileInfo) =
        fileInfo.Exists

    let openText (fileInfo: FileInfo) =
        fileInfo.OpenText()

    let readAllText (fileInfo: FileInfo) =
        async {
            use reader = fileInfo |> openText
            return! reader.ReadToEndAsync() |> Async.AwaitTask
        }

    let readLines (fileInfo: FileInfo) =
        asyncSeq {
            use reader = fileInfo |> openText
            while not reader.EndOfStream do
                let! line = reader.ReadLineAsync() |> Async.AwaitTask
                yield line
        }   

    let readAllLines (fileInfo: FileInfo) =
        fileInfo |> readLines |> AsyncSeq.toArrayAsync

    let openBytes (fileInfo: FileInfo) =
        fileInfo.FullName |> File.openBytes

    let readBytes (fileInfo: FileInfo) =
        fileInfo.FullName |> File.readBytes

    let readBlock blockSize (fileInfo: FileInfo) =                  
        fileInfo.FullName |> File.readBlock blockSize

    let readAllBytes (fileInfo: FileInfo) =
        fileInfo.FullName |> File.readAllBytes