namespace JQ
open FParsec
open JSON
module JQ = 

    (*printfn "Hello from F#"*)
    let str = System.Console.In.ReadToEnd()
    run jvalue str |> printfn "%A"
