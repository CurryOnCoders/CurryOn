namespace CurryOn.Validation

/// Validation-Rule Evaluation Expression, a Predicate from 'value -> bool that 
/// tests the condition tupled with a function from 'value -> 'error that constructs
/// the failure event if the condition is not satisfied
type ValidationRuleExpr<'value, 'error> = ('value -> bool) * ('value -> 'error)

/// The result of evaluating a Validation Rule with a specific value,
/// representing whether the value satisfies the Validation Rule, or
/// violates the Validation Rule, and the relevant error
type ValidationRuleEvaluationResult<'error> =
    | MandatoryRuleSatisfied
    | MandatoryRuleViolated of 'error
    | OptionalRuleSatisfied
    | OptionalRuleViolated of 'error
    member result.IsSatisfied =
        match result with
        | MandatoryRuleSatisfied | OptionalRuleSatisfied -> true
        | _ -> false
    member result.IsViolated = 
        result.IsSatisfied |> not
    member result.IsMandatory =
        match result with
        | MandatoryRuleSatisfied | MandatoryRuleViolated _ -> true
        | _ -> false
    member result.IsOptional = 
        result.IsMandatory |> not
    member result.Error =
        match result with
        | MandatoryRuleViolated error | OptionalRuleViolated error -> Some error
        | _ -> None

/// A Validation Rule defines a condition, either mandatory (Must) or optional (Should)
/// against which a particular value can be tested.  If the condition is not satisfied, a 
/// specific error is returned that ecapsulates the unsatisfied condition of the rule
type ValidationRule<'value, 'error> =
    | Must of ValidationRuleExpr<'value,'error>
    | Should of ValidationRuleExpr<'value,'error>
    member rule.Evaluate value =
        match rule with
        | Must (eval, error) -> 
            if eval value
            then MandatoryRuleSatisfied
            else MandatoryRuleViolated <| error value
        | Should (eval, error) ->
            if eval value
            then OptionalRuleSatisfied
            else OptionalRuleViolated <| error value

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ValidationRule =
    /// Create a required business rule with the given condition and error
    let inline require eval error =
        [Must (eval, error)]

    /// Create a suggested business rule with the given condition and error
    let inline suggest eval warn =
        [Should (eval, warn)]

    /// Add a required business rule with the given condition and error
    /// to an existing list of business rules
    let inline also eval error rules =
        rules @ (require eval error)

    /// Add a suggested business rule with the given condition and error
    /// to an existing list of business rules
    let inline should eval warn rules =
        rules @ (suggest eval warn)

    /// Test if a given business rule is required
    let inline isRequired rule =
        match rule with
        | Must _ -> true
        | _ -> false

    /// Test if a given business rule is suggested
    let inline isSuggested rule =
        match rule with
        | Should _ -> true
        | _ -> false

    /// Apply a specific Validation Rule to the given value
    let inline apply value (rule: ValidationRule<_,_>) =
        rule.Evaluate value

    /// Evaluate whether the given value matches the specified business rule.
    /// Returns 'true' if the rule is satisfied, otherwise 'false'
    let inline satisfies value rule =
        match rule |> apply value with
        | MandatoryRuleSatisfied | OptionalRuleSatisfied -> true
        | _ -> false

    /// Evaluate whether the given value matches the specified business rule.
    /// Returns 'true' if the rule is violated, otherwise 'false'
    let inline violates value = satisfies value >> not

    /// Generate the error raised by the specified ValidationRule 
    /// failing with the given value
    let inline fail value rule =
        match rule with
        | Must (_,error) -> error value
        | Should (_,warn) -> warn value

/// Define a set of ValidationRules that are applied together
type ValidationRuleBuilder () =
    member inline __.Yield _ = List.empty
    [<CustomOperation("rule")>]
    member inline __.Must (rules: ValidationRule<'value,'error> list, eval: 'value -> bool, error: 'value -> 'error) = 
        rules |> ValidationRule.also eval error
    [<CustomOperation("suggestion")>]
    member inline __.Should (rules: ValidationRule<'value,'error> list, eval: 'value -> bool, warn: 'value -> 'error) = 
        rules |> ValidationRule.should eval warn

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ValidationRuleBuilder =
    let validation = ValidationRuleBuilder()