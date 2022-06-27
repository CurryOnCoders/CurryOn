namespace CurryOn.Identifiers

open System

module NanoId =
    [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
    let numberInBits = sizeof<int> * 8

    [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
    let inline countLeadingZeros (value: int) =
        let mutable x = value
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        //count the ones
        x <- x - (x >>> 1 &&& 0x55555555)
        x <- (x >>> 2 &&& 0x33333333) + (x &&& 0x33333333)
        x <- (x >>> 4) + (x &&& 0x0f0f0f0f)
        x <- x + (x >>> 8)
        x <- x + (x >>> 16)
        numberInBits - (x &&& 0x0000003f) //subtract # of 1s from 32

    module Factory =
        let inline create (alphabet: string) (size: int) =    
            if alphabet |> isNull || alphabet.Length < 1 then
                raise (ArgumentOutOfRangeException "Alphabet contain at least one symbol")

            if size < 1 then
                raise (ArgumentOutOfRangeException "Size must be a positive integer")

            let leadingZeros = countLeadingZeros((alphabet.Length - 1) ||| 1)
            let mask = (2 <<< 31 - leadingZeros) - 1
            let step = Math.Ceiling(1.6 * (float (mask * size)) / (float alphabet.Length)) |> int

            let rec buildId (idBuilder: char[]) (bytes: byte[]) count index =
                if count = size then
                    String(idBuilder)
                elif index = bytes.Length then
                    buildId idBuilder bytes count 0
                else
                    if index = 0 then
                        RandomNumberGenerator.getBytes bytes
            
                    let alphabetIndex = (int bytes[index]) &&& mask
                    if alphabetIndex < alphabet.Length then
                        idBuilder[count] <- alphabet[alphabetIndex]
                        buildId idBuilder bytes (count + 1) (index + 1)
                    else
                        buildId idBuilder bytes count (index + 1)
            
            fun () ->                
                let idBuilder = Array.zeroCreate<char> size
                let bytes = Array.zeroCreate<byte> step
                buildId idBuilder bytes 0 0

    let inline generate alphabet size =
        let factory = Factory.create alphabet size
        factory ()

    module Default =
        [<Literal>]
        let Alphabet = "_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        [<Literal>]
        let Size = 21

        let private factory = Factory.create Alphabet Size

        let generate () = factory ()
