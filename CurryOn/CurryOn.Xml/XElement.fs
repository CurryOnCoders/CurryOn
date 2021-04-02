namespace CurryOn.Xml

open System.Xml.Linq
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module XElement =    
    let parse xml =
        XElement.Parse(xml)

    let tryElement name (xml: XElement) =
        match xml.Element(xname name) with
        | null -> None
        | element -> Some element

    let elements name (xml: XElement) =
        xml.Elements(xname name)

    let descendents name (xml: XElement) =
        xml.Descendants(xname name)

    let filter (f: XElement -> bool) (xml: XElement) =
        xml.Elements()
        |> Seq.filter f

    let filterAllDescendants (f: XElement -> bool) (xml: XElement) =
        xml.Descendants()
        |> Seq.filter f

    let tryFind (f: XElement -> bool) (xml: XElement) =
        xml.Descendants()
        |> Seq.tryFind f

    let value (xml: XElement) = 
        xml.Value

    let tryAttribute name (xml: XElement) =
        match xml.Attribute(xname name) with
        | null -> None
        | attribute -> Some attribute