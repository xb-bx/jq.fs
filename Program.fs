module JQMain

open FParsec
open System
open JSON
open JQ
open JQParser

let jsonToString (jv: JValue) : (string * (ConsoleColor option)) list =
    let indentation indentLvl = String.replicate indentLvl

    let listMapAllMapLast fall flast xs =
        let rec loop xs acc =
            match xs with
            | [] -> acc
            | head :: [] -> acc @ [ flast head ]  
            | head :: tail -> loop tail (acc @ [ fall head ])

        loop xs []

    let rec jsonToString' (indentLvl: int) (postfix: string) jv : (string * (ConsoleColor option)) list =
        let indent indentLvl s = (indentation indentLvl "  ") + s

        let colored (color: ConsoleColor option) (indentLvl: int) (s: string) : string * ConsoleColor option =
            (indent indentLvl s), color

        let default' = colored None
        let green = ConsoleColor.Green |> Some |> colored
        let blue = ConsoleColor.Blue |> Some |> colored

        match jv with
        | JNull -> [ default' indentLvl ("null" + postfix) ]
        | JNumber num -> [ default' indentLvl ((num.ToString()) + postfix) ]
        | JBool true -> [ default' indentLvl ("true" + postfix) ]
        | JBool false -> [ default' indentLvl ("false" + postfix) ]
        | JString str -> [ green indentLvl (("\"" + str + "\"") + postfix) ]
        | JArray [] -> [ default' indentLvl ("[]" + postfix) ]
        | JArray arr ->
            [ [ default' indentLvl "[\n" ]
              (arr
               |> listMapAllMapLast (jsonToString' (indentLvl + 1) ",\n") (jsonToString' (indentLvl + 1) "\n")
               |> List.concat)
              [ default' indentLvl ("]" + postfix) ] ]
            |> List.concat
        | JObject fields ->
            let field postfix indentLvl (name, value) : (string * (ConsoleColor option)) list =
                let head :: tail = jsonToString' (indentLvl) postfix value
                let valueList = [ ((fst head).TrimStart()), snd head ] @ tail
                [ ("\"" + name + "\": ") |> blue indentLvl ] @ valueList

            let fieldsList =
                fields
                |> Map.toList
                |> listMapAllMapLast (field ",\n" (indentLvl + 1)) (field "\n" (indentLvl + 1))
                |> List.concat

            [ [ default' indentLvl "{\n" ]
              fieldsList
              [ default' indentLvl ("}" + postfix) ] ]
            |> List.concat




    jsonToString' 0 "" jv

let getSuccess p =
    match p with
    | Success(x, _, _) -> x

let identifier name next = Identifier(name, next)
let selectFromArray from selec = SelectFromArray(from, selec)
let index i next = ArrayElement(i, next)

let printColored ((s, col): string * ConsoleColor option) =
    match col with
    | Some(c) -> Console.ForegroundColor <- c
    | None -> Console.ResetColor()

    Console.Write(s)

type JQErrors =
    | JQError of JQError
    | JsonError of ParserError

let parserResultToResult pr =
    match pr with
    | Success(x, _, _) -> Result.Ok x
    | Failure(_, p, _) -> p |> JsonError |> Result.Error

[<EntryPoint>]
let main args =
    let str = System.Console.In.ReadToEnd()
    let res = run jvalue str

    res
    |> parserResultToResult
    |> Result.iter (jsonToString >> (List.iter printColored))
    let jobj = res |> parserResultToResult |> Result.defaultValue (JNull)

    printfn ""

    run selector args[0]
    |> parserResultToResult 
    |> Result.bind (fun selec -> (evaluateSelector selec (Single jobj) |> Result.mapError JQError ))
    |> Result.map (jqToJv)
    |> Result.iter (jsonToString >> List.iter printColored)

    0
(*printfn "%A" objec*)
(*objec |> jsonToString |> List.iter printColored*)
