namespace CryptoChallenge

module Set1Problem6 = 
    open BasicFunctions
    let p6_msgA = Text2Bytes "this is a test"
    let p6_msgB = Text2Bytes "wokka wokka!!!"
    let testEditDistance = HammingDistance p6_msgA p6_msgB