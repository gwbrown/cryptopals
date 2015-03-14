namespace CryptoChallenge

module Set1Problem3 =
    open BasicFunctions

    let p3_msg = HexString2Bytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

    let p3_msgList = 
        Seq.map (fun (b:byte) -> (SingleByteXOR p3_msg b, b)) AllPossibleBytes 
            |> Seq.map (fun (msg, b) -> (Bytes2String msg, b))
            |> Seq.map (fun (msg, b) -> (ScoreText msg, msg, b))
            |> Seq.sortBy (fun (score,msg, b) -> score) 
            |> Seq.toArray
            |> Seq.take 5
    let p3_solution =
        let (score, msg, key) = (FindBestSingleByteXORMatch p3_msg)
        printfn "Message: \"%s\", Score: %f, Key: %s" msg score (Bytes2String [|key|])