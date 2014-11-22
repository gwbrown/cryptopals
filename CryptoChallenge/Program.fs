// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace CryptoChallenge

module Main =
    open BasicFunctions
    open System
    [<EntryPoint>]
    let main argv = 
        Set1Problem3.p3_solution
        printfn "Press a key to end program."
        let endofapp = Console.ReadKey()
        0 // return an integer exit code