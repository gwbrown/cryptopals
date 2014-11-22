// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace CryptoChallenge

module Main =
    open BasicFunctions
    open System
    open Set1Problem4
    [<EntryPoint>]
    let main argv = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        // Begin user code.
        p4_solution 
        // End user code.
        printfn "Press a key to end program."
        stopWatch.Stop()
        printfn "%f seconds" stopWatch.Elapsed.TotalSeconds
        let endofapp = Console.ReadKey()
        0 // return an integer exit code