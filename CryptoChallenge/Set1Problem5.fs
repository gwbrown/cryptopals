namespace CryptoChallenge

module Set1Problem5 = 
    open BasicFunctions
    let p5_msg = String2Bytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    let p5_key = String2Bytes "ICE"
    let p5_solution =
        RepeatingXOR p5_msg p5_key |> Bytes2Hex