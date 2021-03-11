namespace CurryOn

open System
open System.Reflection

[<AutoOpen>]
module Extensions =
    let internal exists : obj -> bool = box >> isNull >> not

    type Type with
        member this.Implements (clrType: Type) =
            clrType.IsAssignableFrom(this)
        member this.Implements<'t> () =
            this.Implements(typeof<'t>)        
        member this.IsInjectable = 
            not (this.IsAbstract || this.IsInterface)
        member this.IsInjectableAs (clrType: Type) =
            this.IsInjectable && this.Implements(clrType)
        member this.IsInjectableAs<'t> () =
            this.IsInjectableAs typeof<'t>        
        member this.IsConstructable() =
            this.GetConstructors() |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0)

    type ICustomAttributeProvider with
        member this.GetAttributes<'a when 'a :> Attribute>(?inherits: bool) =
            this.GetCustomAttributes(typeof<'a>, inherits |> Option.defaultValue false)
            |> Array.map unbox<'a>


module Attribute =
    let tryGet<'attribute when 'attribute :> Attribute> (element: ICustomAttributeProvider) =
        element.GetAttributes<'attribute>(true) |> Seq.tryHead

    let hasAttribute<'a when 'a :> Attribute> (element: ICustomAttributeProvider) =
        element.GetAttributes<'a>().Length > 0