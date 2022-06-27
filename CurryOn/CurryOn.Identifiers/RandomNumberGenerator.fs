namespace CurryOn.Identifiers

open System.Security.Cryptography

module RandomNumberGenerator =
    let private cryptoRandom = RandomNumberGenerator.Create()

    let getBytes (array: byte[]) =
        cryptoRandom.GetBytes(array)

