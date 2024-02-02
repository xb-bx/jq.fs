open FParsec
open JQ.JSON
open System

let tuppled f x =
    f (fst x) (snd x)

let jsonToString (jv: JValue): (string * (ConsoleColor option)) list =
    let indentation indentLvl = 
        String.replicate indentLvl
    let rec jsonToString' (indentLvl: int) (postfix: string) jv : (string * (ConsoleColor option)) list   = 
        let indent indentLvl s = (indentation indentLvl "  ") + s 
        let colored  (color: ConsoleColor option) (indentLvl) (s: string): string * ConsoleColor option  = (indent indentLvl s), color
        let default' = colored None
        let green = ConsoleColor.Green |> Some |> colored
        let blue = ConsoleColor.Blue |> Some |> colored
        let field indentLvl name value  =
            let name = name |> sprintf "\"%s\": " |> blue indentLvl
            let head :: tail = jsonToString' indentLvl "" value
            let fld = [(((head |> fst).TrimStart()), head |> snd)] @ tail 
            [name] @ fld
    

        match jv with
        | JNull -> [default' indentLvl ("null" + postfix)]
        | JNumber num -> [ default' (indentLvl) ((num.ToString()) + postfix) ]
        | JBool true -> [ default' (indentLvl)("true" + postfix) ]
        | JBool false -> [ default' (indentLvl)("false" + postfix) ]
        | JString str -> [ green (indentLvl) (("\"" + str + "\"") + postfix) ]
        | JArray arr -> 
            let joined = (arr |> List.mapi (fun i -> (jsonToString' (indentLvl + 1) (if i = (List.length arr - 1) then "\n" else ",\n"))) |> List.concat)
            [[ default' (indentLvl) "[\n" ]; joined; [ default' (indentLvl) ("]" + postfix)]] |> List.concat
        | JObject o ->
            let fields = o |> Map.toList |> List.map (tuppled (field (indentLvl + 1)))
            let len = List.length fields
            let rec replaceTail f x =
                match x with 
                | head :: [] -> [f head]
                | head :: tail ->  [head] @ replaceTail f tail
                | _ -> []
            
            let fieldStrings = fields |> List.mapi (
                fun i x -> 
                    if i = len - 1 then 
                        (replaceTail (fun (str, color) -> (str + "\n", color)) x)
                    else 
                        (replaceTail (fun (str, color) -> (str + ",\n", color)) x)
                )
            let fieldStrings = fieldStrings |> List.concat
            ([default' indentLvl "{\n"]) @ fieldStrings @ ([default' indentLvl ("}" + postfix)])
            (*[default' "{\n"; fieldStrings |> List.concat*)


            
            
            
    jsonToString' 0 "" jv

let str = System.Console.In.ReadToEnd()
let res = run jvalue str 
let decolorize ((s, col)) =
    s, None
let printColored ((s, col) : string * ConsoleColor option) =
    match col with
    | Some(c) -> Console.ForegroundColor <- c
    | None -> Console.ResetColor()
    Console.Write(s)
let dump o = 
    printfn "%A" o
    o
match res with 
| Success (objec, _, _) -> objec |> jsonToString |> dump |> List.iter printColored


