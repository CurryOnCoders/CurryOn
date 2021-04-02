namespace CurryOn.Xml

open System.Xml.Linq

module XAttribute =
    let name (attribute: XAttribute) =
        attribute.Name

    let value (attribute: XAttribute) =
        attribute.Value

    let set value (attribute: XAttribute) =
        attribute.SetValue(value)

           