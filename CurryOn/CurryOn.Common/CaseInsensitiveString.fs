namespace CurryOn

open System

[<Struct; CustomEquality; CustomComparisonAttribute>] 
type CaseInsensitiveString = 
    private CaseInsensitiveString of string
    with 
        override this.Equals (o) =
            let (CaseInsensitiveString original) = this
            match o with
            | :? string as s -> 
                original.Equals(s, StringComparison.CurrentCultureIgnoreCase)
            | :? CaseInsensitiveString as cis -> 
                let (CaseInsensitiveString s) = cis
                original.Equals(s, StringComparison.CurrentCultureIgnoreCase)
            | other ->
                original.Equals(other)
        override this.GetHashCode () =
            let (CaseInsensitiveString original) = this
            original.ToLower().GetHashCode()
        interface IComparable with
            member this.CompareTo (o) =
                let (CaseInsensitiveString original) = this
                let lower = original.ToLower()
                match o with
                | :? string as s -> 
                    lower.CompareTo(s.ToLower())
                | :? CaseInsensitiveString as cis -> 
                    let (CaseInsensitiveString s) = cis
                    lower.CompareTo(s.ToLower())
                | other ->
                    lower.CompareTo(other)

module CaseInsensitiveString =
    let create str =
        str |> CaseInsensitiveString

    let value (CaseInsensitiveString str) = str

