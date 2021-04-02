namespace CurryOn.IO

open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DirectoryInfo =
    let exists (directory: DirectoryInfo) =
        directory.Exists

    let create (directory: DirectoryInfo) =
        if not directory.Exists
        then directory.Create()

    let getSubdirectories (directory: DirectoryInfo) =
        directory.EnumerateDirectories()

    let getAllSubdirectories (directory: DirectoryInfo) =
        directory.EnumerateDirectories( "*", SearchOption.AllDirectories)

    let getFiles (directory: DirectoryInfo) =
        directory.EnumerateFiles()

    let getAllFiles (directory: DirectoryInfo) =
        directory.EnumerateDirectories("*", SearchOption.AllDirectories)

    let filter criteria (directory: DirectoryInfo) =
        match criteria with
        | AllContents pattern ->
            directory.EnumerateFileSystemInfos(pattern, SearchOption.AllDirectories)
            |> Seq.choose FileSystem.resolve
        | AllTopLevelContents pattern ->
            directory.EnumerateFileSystemInfos(pattern, SearchOption.TopDirectoryOnly)
            |> Seq.choose FileSystem.resolve
        | FilesOnly pattern ->
            directory.EnumerateFiles(pattern, SearchOption.AllDirectories)
            |> Seq.map FileSystemSearchResult.File
        | TopLevelFilesOnly pattern ->
            directory.EnumerateFiles(pattern, SearchOption.TopDirectoryOnly)
            |> Seq.map FileSystemSearchResult.File
        | DirectoriesOnly pattern ->
            directory.EnumerateDirectories(pattern, SearchOption.AllDirectories)
            |> Seq.map FileSystemSearchResult.Directory
        | TopLevelDirectoriesOnly pattern ->
            directory.EnumerateDirectories(pattern, SearchOption.TopDirectoryOnly)
            |> Seq.map FileSystemSearchResult.Directory
