namespace CurryOn.IO

open CurryOn
open FSharp.Control
open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FileInfo =
    let exists (fileInfo: FileInfo) =
        fileInfo.Exists

    let name (fileInfo: FileInfo) =
        let segments = fileInfo.Name.Split('.')

        match segments with
        | [| name |] ->
            name
        | segments ->
            segments
            |> Seq.take (segments.Length - 1)
            |> String.join "."

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

    let openStream mode (fileInfo: FileInfo) =
        match mode with
        | ReadOnly ->
            fileInfo.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        | WriteOnly ->
            fileInfo.Open(FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read)
        | ReadAndWrite ->
            fileInfo.Open(FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
        | Exclusive ->
            fileInfo.Open(FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)

    let openBytes (fileInfo: FileInfo) =
        fileInfo.FullName |> File.openBytes

    let readBytes (fileInfo: FileInfo) =
        fileInfo.FullName |> File.readBytes

    let readBlock blockSize (fileInfo: FileInfo) =                  
        fileInfo.FullName |> File.readBlock blockSize

    let readAllBytes (fileInfo: FileInfo) =
        fileInfo.FullName |> File.readAllBytes

    let openWrite (fileInfo: FileInfo) =
        new StreamWriter(fileInfo.FullName)

    let writeText (content: string) (fileInfo: FileInfo) =
        fileInfo.FullName |> File.writeText content

    let writeBytes (content: byte []) (fileInfo: FileInfo) =
        async {
            use writer = new BinaryWriter(fileInfo |> openStream WriteOnly)
            writer.Write(content)
            writer.Flush()
        }