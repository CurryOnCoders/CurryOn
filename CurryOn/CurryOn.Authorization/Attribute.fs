namespace CurryOn.Authorization

open System

/// Attribute used to filter methods based on the given role.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method, Inherited = true, AllowMultiple = true)>] 
type AuthorizeAttribute(roles: string []) =
    inherit Attribute()
    new (role: string) =
        let roles = role.Split([| ','; ';' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim())
        AuthorizeAttribute(roles)
    new () = AuthorizeAttribute([||])

    member __.Roles = roles
