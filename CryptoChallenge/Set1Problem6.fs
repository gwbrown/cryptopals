namespace CryptoChallenge

module Set1Problem6 = 
    open BasicFunctions
    let p6_msgA = Text2Bytes "this is a test"
    let p6_msgB = Text2Bytes "wokka wokka!!!"
    let testEditDistance = HammingDistance p6_msgA p6_msgB
    let tries = 4

    let p6_filename = "../../problem6.txt"
    let p6_solution file = 
        let p6_text = readFile file
        let encoded_bytes = Base642Bytes p6_text
        let possible_key_lengths = [| 1 .. (debugPrint "message length" encoded_bytes.Length) / 4 |]
        let keysize_scores = Array.sortBy (fun t -> snd t) <| Array.Parallel.map (FindKeysizeScore encoded_bytes) possible_key_lengths
        let keysizes_to_try = debugPrint "keysizes_to_try" <| (keysize_scores |> Seq.take tries |> Seq.toArray) |> Seq.map fst
        let chunked_messages = Array.Parallel.map (BuildSingleByteKeyBlocks encoded_bytes) <| Seq.toArray keysizes_to_try
        let solve_message (chunked_msg : seq<seq<byte>>) : seq<byte> = 
            Seq.toArray chunked_msg |> Array.Parallel.map FindBestSingleByteXORMatch
                |> Seq.map (fun (_,_,key_byte) -> key_byte)
        let solved_keys = Seq.map solve_message chunked_messages
//        let solved_messages = Seq.toArray solved_keys |> Array.Parallel.map (RepeatingXOR encoded_bytes)
        for key in solved_keys do
            printfn "\n\n----------------------\n"
            printfn "Key: %s\n" (Bytes2String key)
            printfn "Message:%s" (RepeatingXOR encoded_bytes key |> Bytes2String)