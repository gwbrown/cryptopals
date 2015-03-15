namespace CryptoChallenge

module Set1Problem1 =
    open BasicFunctions

    let p1_msg = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        //"I'm killing your brain like a poisonous mushroom"
    let p1_solution = Hex2Bytes p1_msg |> Bytes2Base64