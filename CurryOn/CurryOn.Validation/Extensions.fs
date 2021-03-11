namespace CurryOn.Validation

open CurryOn
open System.Collections.Generic

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Validation =
    /// Apply the given list of business rules to the specified value, executing the constructor
    /// if none of the required rules are violated, and returning an OperationResult with any
    /// warnings or errors resulting from the validations
    let inline validate<'a,'b,'e> (ctor: 'a -> 'b) (validations: ValidationRule<'a, 'e> list) (value: 'a) =
        let results = validations |> List.map (ValidationRule.apply value)
        let errors = results |> List.filter (fun result -> result.IsMandatory) |> List.choose (fun result -> result.Error)
        let warnings = results |> List.filter (fun result -> result.IsOptional) |> List.choose (fun result -> result.Error)
        match errors, warnings with
        | [], [] -> value |> ctor |> Successful |> Ok
        | [], events -> {Value = ctor value; Warnings = events} |> WithWarnings |> Ok
        | _ -> Error (errors @ warnings)

    /// Apply the given list of business rules to validate a string, executing the constructor
    /// if none of the required rules are violated, and returning an OperationResult with any
    /// warnings or errors resulting from the validations
    let inline validateString<'b,'e> (ctor: string -> 'b) (validations: ValidationRule<string, 'e> list) (value: string) =
        validate ctor validations value

    /// Apply the given list of business rules to validate a number, executing the constructor
    /// if none of the required rules are violated, and returning an OperationResult with any
    /// warnings or errors resulting from the validations
    let inline validateNumber<'a,'b,'e when 'a : comparison> ctor validations = 
        validate<'a,'b,'e> ctor validations

    /// Apply the given list of business rules to validate a number, executing the constructor
    /// if none of the required rules are violated, and returning an OperationResult with any
    /// warnings or errors resulting from the validations
    let inline validateCollection<'a,'b,'c,'e when 'a :> IEnumerable<'b>> ctor validations = 
        validate<'b,'c,'e> ctor validations


[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Extensions =
    /// Create an option type from a reference type using the given null check and contstructor
    let createOptionalType nullCheck f value =
        if value |> nullCheck
        then None |> ValidatedResult.success
        else f value |> ValidatedResult.map Some

    /// Create an option type from a reference type using the given contstructor
    let createOptional f value =
        createOptionalType isNull f value

    /// Create an option type from a string type using the given contstructor
    let createOptionalString f value =
        createOptionalType isNullOrEmpty f value

    /// Create an option type from a nullable type using the given constructor
    let createNullable f (nullable: System.Nullable<_>) =
        if nullable.HasValue
        then f nullable.Value |> ValidatedResult.map Some
        else None |> ValidatedResult.success

    /// Convert an optional value to a reference type
    let optionalValue f value =
        value |> Option.map f |> Option.toObj

    /// Convert an optional value to a nullable type
    let nullableValue f value =
        value |> Option.map f |> Option.toNullable

    /// Create a list of a validated type from an array of unvalidated input and a given constructor
    let createList f values =
        if values |> isNull
        then ValidatedResult.success []
        else values |> Seq.map f |> ValidatedResult.join

    /// Create an array of an unvalidated type from a list of validated objects
    let arrayValue f values =
        match values with
        | [] -> [||]
        | _ -> values |> List.map f |> List.toArray

    /// Create a set of unique validated objects from an array of unvalidated input and a given constructor
    let createSet f values =
        if values |> isNull
        then ValidatedResult.success Set.empty
        else values |> Seq.map f |> ValidatedResult.join |> ValidatedResult.map Set.ofList

    /// Create an array of an unvalidated type from a set of validated objects
    let setValues f values =
        values |> Set.map f |> Set.toArray 

    /// Creates a union from a string
    let createUnion<'union, 'error> (fError: string -> 'error) (value: string) : ValidatedResult<'union, 'error> =
        match value |> Union.tryParse<'union> with
        | Some result -> result |> ValidatedResult.success
        | None -> ValidatedResult.error (fError value) 

    /// Converts a union into a string value
    let unionValue<'union> (value: 'union) =
        value |> Union.getName |> String.lowercase

    /// Pre-validate a string field by checking it against a regular expression
    let preValidate regex validate error value =
        if Rules.regex regex value then
            value |> validate
        else
            ValidatedResult.error <| error value

    /// Pre-validate a string that will be used as a decimal value
    let validateDecimal validate error value =
        value |> preValidate "^-?[\d]+(\.[\d]{1,3})?$" (decimal >> validate) error
            

