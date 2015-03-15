namespace CryptoChallenge

module Set1Problem2 =
    open BasicFunctions

    let p2_msg = Hex2Bytes "1c0111001f010100061a024b53535009181c" //"the kid don't play"
    let p2_key = Hex2Bytes "686974207468652062756c6c277320657965" //"hit the bull's eye"

    let p2_solution = XORBytes p2_msg p2_key |> Bytes2String