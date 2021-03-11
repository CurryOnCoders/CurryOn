namespace CurryOn.Validation

open System
open FSharp.Reflection

[<Struct>]
type ValidatedWithWarnings<'a, 'event> =
    {
        Value: 'a
        Warnings: 'event list
    }

type Validated<'a, 'event> =
    | Successful of 'a
    | WithWarnings of ValidatedWithWarnings<'a, 'event>
    member this.Value =
        match this with
        | Successful value -> value
        | WithWarnings result -> result.Value
    member this.Warnings =
        match this with
        | Successful _ -> []
        | WithWarnings result -> result.Warnings

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Interface ||| AttributeTargets.Struct, AllowMultiple = false)>]
type ValidatedAttribute (dto: Type option) =
    inherit Attribute ()
    new () = ValidatedAttribute(None)
    new (dto: Type) = ValidatedAttribute(Some dto)
    member __.DTO = dto

type ValidatedResult<'result, 'event> = Result<Validated<'result, 'event>, 'event list>

module ValidatedResult =
    let inline success value = Ok (Successful value)

    let inline warning value warning = Ok (WithWarnings { Value = value; Warnings = [warning] })

    let inline error e : Result<Validated<_,_>, _> = Error [e]

    let internal zero () = success ()

    let internal convertExceptionToResult<'result,'event> (ex: exn) =
        let union = FSharpType.GetUnionCases(typeof<Result<'result, 'event>>)
        if typeof<'event>.IsAssignableFrom(typeof<exn>)
        then FSharpValue.MakeUnion(union.[1], [|[ex] |> box|]) |> unbox<ValidatedResult<'result, 'event>>
        elif FSharpType.IsUnion typeof<'event>
        then let cases = FSharpType.GetUnionCases(typeof<'event>)
             match cases |> Seq.tryFind (fun case -> case.GetFields().Length = 1 && case.GetFields().[0].PropertyType.IsAssignableFrom(typeof<exn>)) with
             | Some case -> FSharpValue.MakeUnion(union.[1], [|[FSharpValue.MakeUnion(case, [|ex |> box|]) |> unbox<'event>] |> box|]) |> unbox<ValidatedResult<'result, 'event>>
             | None -> raise ex
        else raise ex

    let internal delay f = fun result ->
        try f result
        with | ex -> convertExceptionToResult ex
                
    let internal run f =
        try f()
        with | ex -> convertExceptionToResult ex

    let bind f result =
        match result with
        | Ok validated ->
            match validated with
            | Successful value -> 
                f value
            | WithWarnings warned ->
                match f warned.Value with
                | Ok success ->
                    match success with
                    | Successful v -> Ok <| WithWarnings { Value = v; Warnings = warned.Warnings }
                    | WithWarnings w -> Ok <| WithWarnings { w with Warnings = warned.Warnings @ w.Warnings }
                | Error errors ->
                    Error (warned.Warnings @ errors)
        | Error errors -> 
            Error errors

    let map f result =
        match result with
        | Ok validated -> 
            match validated with
            | Successful value -> Ok (Successful (f value))
            | WithWarnings warned -> Ok (WithWarnings { Value = f warned.Value; Warnings = warned.Warnings })
        | Error errors ->
            Error errors

    let mapError f result =
        match result with
        | Ok validated -> 
            match validated with
            | Successful value -> success value
            | WithWarnings warned -> Ok <| WithWarnings {Value = warned.Value; Warnings = warned.Warnings |> List.map f}
        | Error errors -> 
            Error (errors |> List.map f)


    let isOk = function
    | Ok _ -> true
    | _ -> false

    let isError = function
    | Error _ -> true
    | _ -> false

    let isSuccessful = function
    | Ok validated ->
        match validated with
        | Successful _ -> true
        | _ -> false
    | _ ->
        false

    let isWarning = function
    | Ok validated ->
        match validated with
        | WithWarnings _ -> true
        | _ -> false
    | Error _ -> 
        false

    let toOption = function
    | Ok validated -> Some validated.Value
    | _ -> None
        
    let ofOption error = function
    | Some value -> Ok (Successful value)
    | None -> Error [error]

    let toResult : ValidatedResult<'a,'e> -> Result<'a,'e list> = function
    | Ok validated ->
        match validated with
        | Successful value -> value
        | WithWarnings warned -> warned.Value
        |> Ok
    | Error errors -> 
        Error errors

    let ofResult : Result<'a,'e> -> ValidatedResult<'a,'e> = function
    | Ok value -> Ok (Successful value)
    | Error error -> Error [error]

    let combine a b =
        match a with
        | Ok validatedA ->
            match b with
            | Ok validatedB ->
                match validatedA with
                | Successful valueA ->
                    match validatedB with
                    | Successful valueB ->
                        Ok (Successful (valueA, valueB))
                    | WithWarnings warnedB ->
                        Ok (WithWarnings { Value = (valueA, warnedB.Value); Warnings = warnedB.Warnings })
                | WithWarnings warnedA ->
                    match validatedB with
                    | Successful valueB ->
                        Ok (WithWarnings { Value = (warnedA.Value, valueB); Warnings = warnedA.Warnings })
                    | WithWarnings warnedB ->
                        Ok (WithWarnings { Value = (warnedA.Value, warnedB.Value); Warnings = warnedA.Warnings @ warnedB.Warnings })
            | Error errorsB ->
                match validatedA with
                | Successful _ -> Error errorsB
                | WithWarnings warnedA -> Error (warnedA.Warnings @ errorsB)
        | Error errorsA ->
            match b with
            | Ok validatedB ->
                match validatedB with
                | Successful _ -> Error errorsA
                | WithWarnings warnedB -> Error (errorsA @ warnedB.Warnings)                    
            | Error errorsB ->
                Error (errorsA @ errorsB)

    let join results =
        results |> Seq.fold (fun acc cur ->
            match acc with
            | Ok validatedAcc ->
                match cur with
                | Ok validatedCur ->
                    match validatedAcc with
                    | Successful accValue ->
                        match validatedCur with
                        | Successful curValue -> accValue @ [curValue] |> success
                        | WithWarnings warnedCur -> Ok (WithWarnings { Value = accValue @ [warnedCur.Value]; Warnings = warnedCur.Warnings })
                    | WithWarnings warnedAcc ->
                        match validatedCur with
                        | Successful curValue -> Ok (WithWarnings { Value = warnedAcc.Value @ [curValue]; Warnings = warnedAcc.Warnings })
                        | WithWarnings warnedCur -> Ok (WithWarnings { Value = warnedAcc.Value @ [warnedCur.Value]; Warnings = warnedAcc.Warnings @ warnedCur.Warnings })
                | Error curErrors ->
                    match validatedAcc with
                    | WithWarnings warnedAcc -> Error (curErrors @ warnedAcc.Warnings)
                    | _ -> Error curErrors
            | Error accErrors ->
                match cur with
                | Ok validatedCur ->
                    match validatedCur with
                    | WithWarnings warnedCur -> Error (accErrors @ warnedCur.Warnings)
                    | _ -> Error accErrors
                | Error curErrors ->
                    Error (accErrors @ curErrors)) (success [])
        
    let apply f x =
        match f with
        | Ok validatedF ->
            match x with
            | Ok validatedX ->
                match validatedF with
                | Successful f ->
                    match validatedX with
                    | Successful x ->
                        Ok (Successful (f x))
                    | WithWarnings w ->
                        Ok (WithWarnings { Value = (f w.Value); Warnings = w.Warnings })
                | WithWarnings wf ->
                    match validatedX with
                    | Successful x ->
                        Ok (WithWarnings { Value = (wf.Value x); Warnings = wf.Warnings })
                    | WithWarnings wx ->
                        Ok (WithWarnings { Value = (wf.Value wx.Value); Warnings = wx.Warnings @ wf.Warnings })
            | Error errorsX ->
                match validatedF with
                | Successful _ -> Error errorsX
                | WithWarnings w -> Error (w.Warnings @ errorsX)
        | Error errorsF ->
            match x with
            | Ok validatedX ->
                match validatedX with
                | Successful _ -> Error errorsF
                | WithWarnings w -> Error (w.Warnings @ errorsF)                    
            | Error errorsX ->
                Error (errorsX @ errorsF)

    let inline (>>=) result f = bind f result

    let inline (>=>) f g x = f x >>= g        

    let inline (<!>) f x = map f x

    let inline (<*>) f x = apply f x

    type internal InternalValidationError =
    | ValidationListIsEmpty
    | ResultTypeIsNotARecord of Type
    | UnhandledExceptionConstructingResult of exn

    type internal PartialValidation<'error> =
        {
            Type: Type
            Value: obj
            Name: string
            IsValid: bool
            Errors: 'error list
            Warnings: 'error list
        }




