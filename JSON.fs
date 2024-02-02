namespace JQ
open FParsec
module JSON = 
    type JValue = 
        | JNull
        | JBool of bool
        | JString of string
        | JNumber of double
        | JObject of Map<string, JValue>
        | JArray of JValue list

    let tryDouble (str: string) = 
        let (ok, res) = System.Double.TryParse str
        if not ok then None
        else Some res
    let join (cs: char list): string =
        new string(cs |> List.toArray)
    let escape c =
        match c with    
        | '\\' -> '\\'
        | '/' -> '/'
        | 'b' -> '\b'
        | 'f' -> '\f'
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | _ -> raise (new System.Exception ())
    let parseHex s =
        System.Convert.ToInt32(s, 16)
    let tup4tolist (((a, b), c), d) = [a; b; c; d]
    let ws = spaces
    let escapeChar: Parser<char, unit> = skipChar '\\' >>. anyOf "\\/bfnrt\"" |>> escape 
    let unicodeChar: Parser<char, unit> = skipString "\\u" >>. hex .>>. hex .>>. hex .>>. hex |>> tup4tolist |>> List.rev |>> join |>> parseHex |>> char
    let jnumber: Parser<JValue, unit> = spaces >>. numberLiteral NumberLiteralOptions.DefaultFloat "number" |>> (fun (x: FParsec.CharParsers.NumberLiteral) -> x.String |> tryDouble |> Option.get) |>> JNumber
    let pString: Parser<string, unit> = spaces >>. skipChar '"' >>. manyTill (unicodeChar <|> escapeChar <|> anyChar) (pchar '"') |>> join 
    let jstring: Parser<JValue, unit> = pString |>> JString
    let word (str: string): Parser<string,unit> = 
        ws >>. pstring str .>> ws 
    let jbool: Parser<JValue, unit> =
        (word "true" >>% JBool true) <|> (word "false" >>% JBool false)
    let jvalue, jvalueRef = createParserForwardedToRef()
    let jarray: Parser<JValue, unit> = 
        word "[" >>. sepBy jvalue (word ",")  .>> word "]" |>> JArray
    let jfield: Parser<(string * JValue), unit> =
        pString .>> word ":" .>>. jvalue
    let jobject: Parser<JValue, unit> =
        word "{" >>. sepBy jfield (word ",") .>> word "}" |>> Map.ofSeq |>> JObject 
    do jvalueRef.Value <- 
        ws >>.
        choice [
            jnumber;
            jstring;
            jbool;
            jarray; 
            jobject;
        ] .>> ws
