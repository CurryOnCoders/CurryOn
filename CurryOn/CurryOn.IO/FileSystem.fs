namespace CurryOn.IO

open System.IO

type FileSystemSearchCriteria =
| AllTopLevelContents of string
| AllContents of string
| TopLevelFilesOnly of string
| FilesOnly of string
| TopLevelDirectoriesOnly of string
| DirectoriesOnly of string

type FileSystemSearchResult =
    | Directory of DirectoryInfo
    | File of FileInfo
    member this.Name = 
        match this with
        | Directory dir -> dir.Name
        | File file -> file.Name
    member this.FullName = 
        match this with
        | Directory dir -> dir.FullName
        | File file -> file.FullName
    member this.IsFolder = 
        match this with
        | Directory _ -> true
        | File _ -> false

type FileAccessMode =
| ReadOnly
| WriteOnly
| ReadAndWrite
| Exclusive

module FileSystem =
    
    let resolvePath (path: string) =
        if File.Exists (path)
        then File (FileInfo path) |> Some
        elif Directory.Exists path
        then Directory (DirectoryInfo path) |> Some
        else None

    let resolve (info: FileSystemInfo) =
        resolvePath info.FullName