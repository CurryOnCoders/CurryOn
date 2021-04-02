open System.IO

let getPackages (folder: string) =
    let directory = DirectoryInfo(folder)
    directory.EnumerateFiles("*.nupkg", SearchOption.AllDirectories)

let copy sideloadingFolder (package: FileInfo) =
    try
        package.CopyTo(sprintf "%s\\%s" sideloadingFolder package.Name, true) |> ignore
    with ex ->
        printfn "Error moving package %s: %s" package.Name ex.Message

let copyAll root target =
    getPackages root
    |> Seq.iter (copy target)

let root = @"C:\repos\CurryOn\CurryOn"
let target = @"C:\nuget"

copyAll root target
