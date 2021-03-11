namespace CurryOn

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Memoization =
    type private MemoizationCache<'a,'b when 'a: equality> (?cacheExpirationPolicy) =
        let cache = 
            match cacheExpirationPolicy with
            | Some policy -> new MemoryCache<string,'b>(policy)
            | None -> new MemoryCache<string,'b>()
        let getKey (key: 'a) = 
            if key |> box |> isNull
            then typeof<'a>.FullName
            else sprintf "%s_%d" typeof<'a>.FullName <| key.GetHashCode()
        member __.TryGetValue key =
            let keyString = getKey key
            cache.Get keyString
        member __.GetOrAdd key getValue =
            let keyString = getKey key
            cache.GetOrAddResult keyString getValue

    let private memoizeWithCache f (cache: MemoizationCache<_,_>) x =
        (fun () -> f x) |> cache.GetOrAdd x            

    /// Create a new function that remembers the results of the given function
    /// for each unique input parameter, returning the cached result instead
    /// of recomputing it each time the function is called, optimizing the execution speed
    /// at the cost of increased memory usage.
    ///
    /// Note:  This optimization should only be used with referentially transparent functions
    let memoize f = 
        new MemoizationCache<_,_>() |> memoizeWithCache f

    /// Create a new function that remembers the results of the given function
    /// for each unique input parameter, returning the cached result instead
    /// of recomputing it each time the function is called, optimizing the execution speed
    /// at the cost of increased memory usage.  Uses a specified Cache Expiration Policy
    /// to limit the amount of time the results are stored, to allow memory to be free'd
    /// after a certain amount of time.
    ///
    /// Note:  This optimization should only be used with referentially transparent functions
    let memoizeWithExpiration policy f = 
        new MemoizationCache<_,_>(policy) |> memoizeWithCache f

