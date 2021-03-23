namespace CurryOn.Authorization

open CurryOn

type AuthorizationError =
| NoAuthorizationHandlerFound
| UnableToVerifyClient of string
| AuthorizationProviderUnavailable
| AuthorizationProviderError of obj
| NotAuthorized
| InvalidKey
| UnexpectedAuthorizationError of exn

[<CLIMutable>]
type RoleList = 
    {
        Roles: string []
    }

type SystemKey = 
    | SystemKey of string

type IClientPrincipal =
    abstract member ClientId: string

type IAuthorizationHandler =       
    abstract member UseHostAuthorization: bool
    abstract member HasRole: string -> IClientPrincipal -> AsyncResult<bool, AuthorizationError>
    abstract member CheckAuthorizedRole: RoleList -> IClientPrincipal -> AsyncResult<string option, AuthorizationError>