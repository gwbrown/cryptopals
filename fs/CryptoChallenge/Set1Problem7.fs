namespace CryptoChallenge

module Set1Problem7 =
    open BasicFunctions

    let filename = "../../problem7.txt"
    let key = String2Bytes "YELLOW SUBMARINE"

    let solution file = 
        let text = readFile file
        let encoded_bytes = Base642Bytes text
        AES_ECB_Decrypt encoded_bytes key |> Bytes2String
