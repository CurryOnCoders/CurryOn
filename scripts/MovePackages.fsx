open System.IO

let getPackages (folder: string) =
    let directory = DirectoryInfo(folder)
    directory.EnumerateFiles("*.nupkg", SearchOption.AllDirectories)

let copy sideloadingFolder (package: FileInfo) =
    try
        let file = package.CopyTo(sprintf "%s\\%s" sideloadingFolder package.Name, true)
        printfn "Copied %s" file.Name
    with ex ->
        printfn "Error moving package %s: %s" package.Name ex.Message

let copyDebug root target =
    getPackages root
    |> Seq.filter (fun p -> p.FullName.Contains("Debug"))
    |> Seq.iter (copy target)

let copyRelease root target =
    getPackages root
    |> Seq.filter (fun p -> p.FullName.Contains("Release"))
    |> Seq.iter (copy (sprintf "%s\\release" target))
    

let root = @"C:\repos\CurryOn\CurryOn"
let target = @"C:\nuget"

//copyDebug root target
copyRelease root target