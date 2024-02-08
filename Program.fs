module JQMain

open Argu
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

type CliArgument = 
    | [<AltCommandLine("-r")>] Raw_Output
    | [<AltCommandLine("-f")>] File of path:string 
    | [<MainCommand; Last>] Filter of filter:string
    (*| Help *)
    interface IArgParserTemplate with
        member s.Usage = 
            match s with 
            | Raw_Output -> "raw output"
            | File _ -> "read from file instead of stdin"
            | Filter _ -> "filter" 
            (*| Help -> "help"*)

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArgument>()

    let args = 
        try 
            parser.Parse()
        with
        | :? Argu.ArguParseException as e when e.ErrorCode = ErrorCode.HelpText -> 
            parser.PrintUsage() |> printfn "%s" 
            exit 0

    let printJson = 
        if args.Contains Raw_Output then 
            fun j ->
                match j with 
                    | JString s -> printfn "%s" s
                    | _ -> jsonToString j |> List.iter printColored
        else 
            jsonToString >> List.iter printColored
            
    let input = args.TryGetResult File |> Option.filter System.IO.File.Exists |> Option.map System.IO.File.ReadAllText |> Option.defaultWith Console.In.ReadToEnd
    let filter = args.TryGetResult Filter
    match run jvalue input |> parserResultToResult with 
    | Result.Ok(jvalue) ->
        if filter.IsNone then 
            printJson jvalue
        else 
            let res = 
                run expression filter.Value
                    |> parserResultToResult 
                    |> Result.bind (fun selec -> (evaluateExpression selec jvalue |> Result.mapError JQError ))
            match res with 
            | Result.Ok(jvalue) -> printJson jvalue
            | Result.Error(err) -> 
                eprintfn "%A" err
                exit 1
            
    | Result.Error(error) -> 
        eprintfn "%A" error 
        exit 1

    0
(*printfn "%A" objec*)
(*objec |> jsonToString |> List.iter printColored*)
