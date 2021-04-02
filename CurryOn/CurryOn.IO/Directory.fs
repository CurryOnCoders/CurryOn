namespace CurryOn.IO

open System.IO

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Directory =
    let exists (path: string) =
        Directory.Exists(path)

    let create (path: string) =
        if not (exists path)
        then Directory.CreateDirectory(path)
        else DirectoryInfo path

    let getSubdirectories (path: string) =
        Directory.EnumerateDirectories(path)

    let getAllSubdirectories (path: string) =
        Directory.EnumerateDirectories(path, "*", SearchOption.AllDirectories)

    let getFiles (path: string) =
        Directory.EnumerateFiles(path)

    let getAllFiles (path: string) =
        Directory.EnumerateDirectories(path, "*", SearchOption.AllDirectories)

    let filter criteria path =
        match criteria with
        | AllContents pattern ->
            Directory.EnumerateFileSystemEntries(path, pattern, SearchOption.AllDirectories)
        | AllTopLevelContents pattern ->
            Directory.EnumerateFileSystemEntries(path, pattern, SearchOption.TopDirectoryOnly)
        | FilesOnly pattern ->
            Directory.EnumerateFiles(path, pattern, SearchOption.AllDirectories)
        | TopLevelFilesOnly pattern ->
            Directory.EnumerateFiles(path, pattern, SearchOption.TopDirectoryOnly)
        | DirectoriesOnly pattern ->
            Directory.EnumerateDirectories(path, pattern, SearchOption.AllDirectories)
        | TopLevelDirectoriesOnly pattern ->
            Directory.EnumerateDirectories(path, pattern, SearchOption.TopDirectoryOnly)
