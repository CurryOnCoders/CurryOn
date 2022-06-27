namespace CurryOn.Identifiers

open System

/// Quid -- Quick Unique Identifier
module Quid =
    module Factory =
        let inline create (alphabet: string) size breakCount separator =
            if alphabet |> isNull || alphabet.Length < 1 then
                raise (ArgumentOutOfRangeException "Alphabet contain at least one symbol")

            if size < 1 then
                raise (ArgumentOutOfRangeException "Size must be a positive integer")

            if breakCount < 1 || breakCount > size then
                raise (ArgumentOutOfRangeException "Break Position must be between 1 and ID Size")

            let leadingZeros = NanoId.countLeadingZeros((alphabet.Length - 1) ||| 1)
            let mask = (2 <<< 31 - leadingZeros) - 1
            let step = Math.Ceiling(1.6 * (float (mask * size)) / (float alphabet.Length)) |> int

            let totalSize = 
                let remainder = size % breakCount
                if remainder = 0 then
                    size + (size / breakCount) - 1
                else
                    size + (size / breakCount)

            let rec buildId (idBuilder: char []) (bytes: byte []) breakPosition count index =
                if count = totalSize then
                    String(idBuilder)
                elif breakPosition = breakCount then
                    idBuilder[count] <- separator
                    buildId idBuilder bytes 0 (count + 1) index
                elif index = bytes.Length then
                    buildId idBuilder bytes breakPosition count 0
                else
                    if index = 0 then
                        RandomNumberGenerator.getBytes bytes
                
                    let alphabetIndex = (int bytes[index]) &&& mask
                    if alphabetIndex < alphabet.Length then
                        idBuilder[count] <- alphabet[alphabetIndex]
                        buildId idBuilder bytes (breakPosition + 1) (count + 1) (index + 1)
                    else
                        buildId idBuilder bytes breakPosition count (index + 1)

            fun () -> 
                let idBuilder = Array.zeroCreate<char> totalSize
                let bytes = Array.zeroCreate<byte> step

                buildId idBuilder bytes 0 0 0     

    let inline generate (alphabet: string) size breakCount separator =
        let factory = Factory.create alphabet size breakCount separator
        factory ()

    module Default =
        [<Literal>]
        let Alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"
        [<Literal>]
        let Size = 18
        [<Literal>]
        let Break = 6
        [<Literal>]
        let Separator = '-'

        let private factory = Factory.create Alphabet Size Break Separator

        let generate () = factory ()