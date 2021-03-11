namespace CurryOn.Validation

open CurryOn
open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rules =
    /// Compose function f with functions g and h, and-ing the results
    let private (>>&) f (g, h) x = 
        let r = f x
        g r && h r
                
    // Regular-Expression based business rule
    let inline regex pattern = 
        let completePattern = 
            if pattern |> Seq.contains '^' || pattern |> Seq.contains '$'
            then Regex(pattern, RegexOptions.Compiled)
            else Regex(sprintf "^%s$" pattern, RegexOptions.Compiled)
        completePattern.IsMatch

    // String Rules
    let private lengthRule op length = String.length >> op length
    let private likeRule op values str = values |> op (fun value -> str |> String.like value)
    let isNotEmpty = String.IsNullOrWhiteSpace >> not
    let isAlpha = String.forall Char.IsLetter
    let isNumber input = match Decimal.TryParse input with
                         | true,_ -> true
                         | _ -> false
    let isNumerical = String.forall Char.IsDigit
    let isAlphanumeric = String.forall Char.IsLetterOrDigit
    let isLength = lengthRule (=)
    let maxLength = lengthRule (>=)
    let minLength = lengthRule (<=)
    let inline lengthBetween min max (value: ^a) = 
        let length = (^a : (member get_Length: unit -> int) value)  
        length >= min && length <= max
    let likeAny: string seq -> string -> bool = likeRule Seq.exists
    let likeAll: string seq -> string -> bool = likeRule Seq.forall

    // Number rules
    let inline private compareZero<'n when 'n: comparison> = LanguagePrimitives.GenericComparison Unchecked.defaultof<'n>
    let isNegative n = n |> (compareZero >> (<) 0)
    let isNotNegative n = n|> (compareZero >> (>=) 0)
    let isNotPositive n = n|> (compareZero >> (<=) 0)
    let isPositive n = n|> (compareZero >> (>) 0)
    let isBetween min max =  id >>& ((>=) max, (<=) min)

    // Equality rules
    let oneOf values value = values |> List.contains value 