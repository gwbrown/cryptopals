namespace CryptoChallenge

module BasicFunctions = 
    open System
    // --- Problem 1 ---
    
    let HexByte2Bytes hex = Byte.Parse (hex, Globalization.NumberStyles.HexNumber)

    let HexString2Bytes hex =
        let rec split2s (str:string) =
            if str.Length = 0 then
                []
            elif str.Length % 2 = 0 then
                str.Substring(0,2) :: split2s (str.Substring 2)
            else
                str.Substring(0,1) :: split2s (str.Substring 1)
        Seq.map HexByte2Bytes <| split2s hex
    let Bytes2Base64 : seq<byte> -> string = Seq.toArray >> Convert.ToBase64String
    let Bytes2String (bytes:seq<byte>) = 
        let bytesArr = Seq.toArray(bytes)
        Text.Encoding.UTF8.GetString (bytesArr, 0, bytesArr.Length)

    // --- Problem 2 ---

    let XORBytes (msg:seq<byte>) (key:seq<byte>) =
        let bytePairs = Seq.zip msg key
        let XORByte (b1, b2) = b1 ^^^ b2
        Seq.map XORByte bytePairs

    // --- Problem 3 ---

    let scoreText (str:string) =
        let averageLetterPoints = 3.85
        let scoreChar ch =
            match ch with
                | 'e' -> 12.70
                | 't' -> 9.05
                | 'a' -> 8.17
                | 'o' -> 7.05
                | 'i' -> 6.97
                | 'n' -> 6.75
                | 's' -> 6.32
                | 'h' -> 6.10
                | 'r' -> 5.99
                | 'd' -> 4.25
                | 'l' -> 4.03
                | 'c' -> 2.78
                | 'u' -> 2.76
                | 'm' -> 2.41
                | 'w' -> 2.36
                | 'f' -> 2.23
                | 'g' -> 2.02
                | 'y' -> 1.97
                | 'p' -> 1.93
                | 'b' -> 1.49
                | 'v' -> 0.98
                | 'k' -> 0.77
                | 'j' -> 0.15
                | 'x' -> 0.15
                | 'q' -> 0.10
                | 'z' -> 0.07
                | ' ' -> 0.0
                | '*' -> -20.0 // I see this a lot in bad messages but never in good ones.
                | _ -> -10.0 // Not sure if this is a good idea, but it seems reasonable.
        Seq.map scoreChar (str.ToLower()) |> Seq.sum |> (fun score -> 
            Math.Abs ((score/(averageLetterPoints * (float str.Length))) - 1.0))
            // Scores based on how far off from "average" letter distribution
            // the string is.  Always positive, closer to 0 is better.

    let buildSingleByteKeyForMsg (b:byte) (msg:seq<byte>) = Seq.map (fun _ -> b) msg

    let possibleBytes = Seq.map (Convert.ToByte:int->byte) [|0 .. 255|]

    let decryptSingleByteXOR (msg:seq<byte>) (b:byte) =
        buildSingleByteKeyForMsg b msg |> XORBytes msg

    // --- Problem 4 ---
    let readLines filePath = IO.File.ReadLines(filePath)

    let findBestSingleByteXORMatch (enc_msg:seq<byte>) :(float * string* byte) = 
        Seq.map (fun (b:byte) -> (decryptSingleByteXOR enc_msg b, b)) possibleBytes 
            |> Seq.map (fun (msg, b) -> (Bytes2String msg, b))
            |> Seq.map (fun (msg, b) -> (scoreText msg, msg, b))
            |> Seq.sortBy (fun (score,msg, b) -> score) 
            |> Seq.toArray
            |> Seq.head