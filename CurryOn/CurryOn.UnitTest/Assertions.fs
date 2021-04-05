namespace CurryOn.UnitTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Diagnostics

[<AutoOpen>]
module Assert =
    let inline equals expected actual = Assert.AreEqual(expected, actual)
    let inline equalTo<'a> (expected: 'a) (actual: 'a) = Assert.AreEqual<'a>(expected, actual)
    let inline notEquals expected actual = Assert.AreNotEqual(expected, actual)
    let inline notEqualTo<'a> (expected: 'a) (actual: 'a) = Assert.AreNotEqual<'a>(expected, actual)
    let inline isTrue value = Assert.IsTrue(value)
    let inline isFalse value = Assert.IsFalse(value)
    let inline greaterThan minimum value = Assert.IsTrue(value > minimum)
    let inline lessThan minimum value = Assert.IsTrue(value < minimum)
    let inline greaterThanOrEqual minimum value = Assert.IsTrue(value >= minimum)
    let inline lessThanOrEqual minimum value = Assert.IsTrue(value <= minimum)
    let inline fail message = Assert.Fail(message)
    let inline failf format = Printf.kprintf Assert.Fail format
    let inline withinRange margin (expected: IConvertible) (actual: IConvertible) = 
        let withinDecimalRange (margin: decimal) (expected: decimal) (actual: decimal) =            
            Assert.IsTrue((expected - actual) / expected < margin)
        withinDecimalRange margin (Convert.ToDecimal expected) (Convert.ToDecimal actual)

    let inline instanceOf<'a> value = Assert.IsInstanceOfType(value, typeof<'a>)

    let isOk = function
        | Ok _ -> ()
        | Error error -> fail <| sprintf "Expected Ok result, instead got Error (%A)" error

    let isError = function
        | Ok value -> fail <| sprintf "Expected Error result, instead got Ok (%A)" value
        | Error _ -> ()

    let debugfn pattern = Printf.ksprintf Debug.WriteLine pattern