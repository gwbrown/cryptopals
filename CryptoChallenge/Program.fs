// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace CryptoChallenge

module Main =
    open BasicFunctions
    open System
    [<EntryPoint>]
    let main argv = 
        for (score, msg, key) in p3_msgList do
            printfn "Message: \"%s\", Score: %f, Key: %s" msg score (Bytes2String [key])
        printfn "Press a key to end program."
        let endofapp = Console.ReadKey()
        0 // return an integer exit code