namespace CryptoChallenge

module Set1Problem5 = 
    open BasicFunctions
    let p5_msg = Text2Bytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    let p5_key = Text2Bytes "ICE"
    let p5_solution =
        RepeatingXOR p5_msg p5_key |> Bytes2Hex