namespace CryptoChallenge

module Set1Problem4 = 
    open BasicFunctions
    let p4_filename = "../../problem4.txt"
    let p4_solution =
        let p4_lines = readLines p4_filename
        let p4_matches = Array.Parallel.map (HexString2Bytes >> findBestSingleByteXORMatch) <| Seq.toArray p4_lines
        let best_p4_matches = Seq.sortBy (fun (score,msg, b) -> score) p4_matches |> Seq.head
        let (score, msg, key) = best_p4_matches
        printfn "Message: \"%s\", Score: %f, Key: %s" msg score (Bytes2String [key])