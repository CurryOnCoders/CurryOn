namespace CurryOn.Cryptography

open CurryOn
open System
open System.Security.Cryptography

[<CLIMutable>]
type SaltedHash =
    {
        Salt: Guid
        Hash: string
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SaltedHash =
    let create salt (hash: byte[]) = 
        {Salt = salt; Hash = Base64.toString hash}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sha256 =
    let private computeHash (bytes: byte []) =
        let sha256 = new SHA256CryptoServiceProvider()
        sha256.ComputeHash(bytes)

    /// Generates a SaltedHash based on the salt provided
    /// Used for generating a hash when you already know the salt
    let getSaltedHash salt =
        sprintf "%A%s" salt
        >> Utf8.toBytes
        >> computeHash 
        >> SaltedHash.create salt

    /// Generates a SaltedHash by generating a new unique Salt and creating a Hash based on the Salt
    let hash input = 
        getSaltedHash (Guid.NewGuid()) input
