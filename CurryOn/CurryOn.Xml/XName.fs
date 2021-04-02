namespace CurryOn.Xml

open System.Xml.Linq

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module XName =
    let get name =
        XName.Get(name)

    let getWithNamespace nameSpace name =
        XName.Get(name, nameSpace)

    let ofString =
        XName.op_Implicit 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<AutoOpen>]
module Utilities =
    let inline xname name = XName.ofString name