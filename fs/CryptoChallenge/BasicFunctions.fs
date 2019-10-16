namespace CryptoChallenge

module BasicFunctions = 
    open System

    let debugPrint name thing = 
        printfn "%s: %+A" name thing
        thing
    // --- Problem 1 ---
    
    let Hex2Byte hex = Byte.Parse (hex, Globalization.NumberStyles.HexNumber)

    let Hex2Bytes hex =
        let rec split2s (str:string) =
            if str.Length = 0 then
                []
            elif str.Length % 2 = 0 then
                str.Substring(0,2) :: split2s (str.Substring 2)
            else
                str.Substring(0,1) :: split2s (str.Substring 1)
        split2s hex |> Seq.map Hex2Byte |> Seq.toArray
    let Bytes2Base64 : byte [] -> string = Convert.ToBase64String
    let Bytes2String (bytes:byte []) = 
        let bytesArr = bytes
        Text.Encoding.UTF8.GetString (bytesArr, 0, bytesArr.Length)

    // --- Problem 2 ---

    let XORBytes (msg:byte []) (key:byte []) =
        let bytePairs = Array.zip msg key
        let XORByte (b1, b2) = b1 ^^^ b2
        Array.map XORByte bytePairs

    // --- Problem 3 ---

    let ScoreText (str:string) =
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
                | '.' -> 0.0
                | '!' -> 0.0
                | ':' -> 0.0
                | _ -> -20.0 // Not sure if this is a good idea, but it seems reasonable.
        Seq.map scoreChar (str.ToLower()) |> Seq.sum |> (fun score -> 
            Math.Abs ((score/(averageLetterPoints * (float str.Length))) - 1.0))
            // Scores based on how far off from "average" letter distribution
            // the string is.  Always positive, closer to 0 is better.

    let BuildSingleByteKeyForMsg (b:byte) (msg:byte []) = Array.map (fun _ -> b) msg

    let AllPossibleBytes = Array.map (Convert.ToByte:int->byte) [|0 .. 255|]

    let SingleByteXOR (msg:byte []) (b:byte) =
        BuildSingleByteKeyForMsg b msg |> XORBytes msg

    // --- Problem 4 ---
    let readLines filePath = IO.File.ReadLines(filePath)
    let readFile filePath = IO.File.ReadAllText(filePath)

    let FindBestSingleByteXORMatch (enc_msg:byte []) : (float * string * byte) = 
        Array.Parallel.map (fun (b:byte) -> (SingleByteXOR enc_msg b, b)) AllPossibleBytes 
            |> Array.Parallel.map (fun (msg, b) -> (Bytes2String msg, b))
            |> Array.Parallel.map (fun (msg, b) -> (ScoreText msg, msg, b))
            |> Array.sortBy (fun (score, _, _) -> score) 
            |> Seq.head

    // --- Problem 5 ---

    let Bytes2Hex bytes = 
        Array.map (fun (x : byte) -> String.Format("{0:X2}", x)) bytes 
            |> String.concat String.Empty

    let String2Bytes (str:string) : byte[] = Text.Encoding.UTF8.GetBytes str

    let BuildRepeatingKeyForMsg (msg:byte []) (key:byte []) : byte [] = 
        let keyA = key
        let matchByte (i:int) _ =
            keyA.[(i % keyA.Length)]
        Array.mapi matchByte msg

    let RepeatingXOR (msg:byte []) (key:byte []) = BuildRepeatingKeyForMsg msg key |> XORBytes msg

    // --- Problem 6 ---

    let Base642Bytes b64 = Convert.FromBase64String(b64)

    let HammingDistance (b1:byte []) (b2:byte []) : int =
        let bytePairs = Array.zip b1 b2
        let HammingWeight (b:byte) = 
            let bitPositions = [| 0 .. 7 |]
            let isSet (byte1:byte) (offset:int) = if (byte1 &&& ((byte 1) <<< offset)) > (byte 0) then 1 else 0
            Array.map (isSet b) bitPositions |> Array.sum
        let HammingDistanceBytes (byte1, byte2) =
            HammingWeight (byte1 ^^^ byte2)
        let distances = Array.map HammingDistanceBytes bytePairs
        Array.sum distances

    let FindKeysizeScore message keysize =
        let byte_numbers = [| 0 .. keysize .. Array.length message-keysize |]
        let getKeysizeScore (msg : byte []) = 
            let getBytes offset = Seq.skip offset msg |> Seq.take keysize |> Seq.toArray
            let blocks = Array.map getBytes byte_numbers
            let ham ((b1, b2) : (byte [] * byte [])) : int = HammingDistance b1 b2
            (Seq.zip (Seq.skip 1 blocks) (Seq.initInfinite (fun _ -> (Seq.head blocks))) 
                |> Seq.toArray 
                |> Array.map ham 
                |> Array.map float 
                |> Array.average) / (float keysize)
        (keysize, getKeysizeScore message)
    
    let BuildSingleByteKeyBlocks (message : byte []) (keysize : int) : byte [] [] =
        let blocks = [| 0 .. keysize - 1 |]
        let byte_numbers offset = [| offset .. keysize .. Array.length message-1 |]
        let buildBlock (msg : byte []) (offset : int) = 
            let getByteNumber num = Seq.skip num msg |> Seq.head
            Array.map getByteNumber (byte_numbers offset)
        Array.map (buildBlock message) blocks

        
    // --- Problem 7 ---
    open System.Security

    let CreateAESProvider (key : byte []) : Cryptography.AesCryptoServiceProvider =
        new Cryptography.AesCryptoServiceProvider(KeySize = 128, Key = key, Mode = Cryptography.CipherMode.ECB, 
                                                      Padding = Cryptography.PaddingMode.Zeros)
    let ApplyAESTransform (transform : Cryptography.ICryptoTransform) (message : byte []) (blockNumber : int) : byte [] =
        let blockBytes = transform.InputBlockSize
        let offset = blockNumber * blockBytes
        let enc_msg : byte [] = Array.create (blockBytes) (byte 0)
        transform.TransformBlock(message, offset, blockBytes, enc_msg, 0) |> ignore
        enc_msg

    let AES_EncryptBlock (message : byte []) (key : byte []) (blockNumber : int) : byte [] = 
        use aes = CreateAESProvider key
        use enc = aes.CreateEncryptor()
        ApplyAESTransform enc message blockNumber

    let AES_DecryptBlock (message : byte []) (key : byte []) (blockNumber : int) : byte [] = 
        use aes = CreateAESProvider key
        use enc = aes.CreateDecryptor()
        ApplyAESTransform enc message blockNumber
        
    let AES_ECB_Encrypt (message : byte []) (key : byte []) : byte [] =
        let AES_Blocksize = 16
        let blockNums = [| 0 .. AES_Blocksize .. message.Length - 1|]
        blockNums
            |> Array.map (AES_EncryptBlock message key)
            |> Array.concat 

    let AES_ECB_Decrypt (message : byte []) (key : byte []) : byte [] =
        let AES_Blocksize = 16
        let blockNums = [| 0 .. (message.Length - 1)/AES_Blocksize|]
        blockNums
            |> Array.map (AES_DecryptBlock message key)
            |> Array.concat 