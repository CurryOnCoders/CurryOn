namespace CurryOn.Xml

open CurryOn.IO
open System.Xml.Linq
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module XDocument =    
    let parse xml =
        XDocument.Parse(xml)

    let load (filePath: string) =
        async {
            let! xml = filePath |> File.readAllText
            return parse xml
        }

    let root (xml: XDocument) =
        xml.Root

    let tryElement name (xml: XDocument) =
        match xml.Root.Element(xname name) with
        | null -> None
        | element -> Some element

    let elements name (xml: XDocument) =
        xml.Root.Elements(xname name)

    let descendents name (xml: XDocument) =
        xml.Descendants(xname name)

    let filter (f: XElement -> bool) (xml: XDocument) =
        xml.Root.Elements()
        |> Seq.filter f

    let filterAllDescendants (f: XElement -> bool) (xml: XDocument) =
        xml.Descendants()
        |> Seq.filter f

    let tryFind (f: XElement -> bool) (xml: XDocument) =
        xml.Descendants()
        |> Seq.tryFind f