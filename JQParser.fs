module JQParser

open FParsec
open JSON
open JQ
let selector, selectorRef = createParserForwardedToRef()
let arrayElement from =
    word "[" >>. (numberLiteral NumberLiteralOptions.DefaultInteger "index" .>> word "]") |>> (fun x -> x.String |> System.Int32.Parse, from) |>> ArrayElement
let selectFromArray from = 
    parse {
        let! _ = word "[]"
        let! selectr = opt selector |>> Option.defaultValue Root
        return SelectFromArray(from, selectr)
    }
let parseId = 
    ws >>. identifier (IdentifierOptions()) .>> ws
    
let identifierWithIndexer previous = 
    parse {
        let! ident = parseId
        let res = Identifier (ident, previous)
        let! indexers = opt (choice [
            selectFromArray res;
            arrayElement res;
        ])
        return indexers |> Option.defaultValue res
        
        
    }
let setFrom value selec = 
    match selec with 
    | Root -> Root
    | ArrayElement(i, _) -> ArrayElement(i, value)
    | Identifier(s, _) -> Identifier(s, value)
    | SelectFromArray(_, s) -> SelectFromArray(value, s)
selectorRef.Value <- 
    parse {
        let! _ = word "."
        let current = Root
        let! parsed = sepBy (choice [ identifierWithIndexer current; selectFromArray current; arrayElement current;  ]) (pchar '.')
        return if parsed.Length = 0 then current else if parsed.Length = 1 then parsed.Head else (parsed.Tail |> List.fold setFrom parsed.Head)
    }
let expression, expressionRef = createParserForwardedToRef()

let numberExpression = 
    numberLiteral NumberLiteralOptions.DefaultFloat "number"
    |>> (fun x -> x.String |> System.Double.Parse)
    |>> NumberExpression
let stringExpression = 
    pString |>> StringExpression
let selectorExpression = 
    selector |>> SelectorExpression
let arrayExpression = 
    word "[" >>. sepBy expression (word ",") .>> word "]"
    |>> ArrayExpression
let objectField = 
    parseId .>> word ":" .>>. expression
let objectExpression = 
    word "{" >>. sepBy objectField (word ",") .>> word "}" 
    |>> Map.ofSeq
    |>> ObjectExpression

    
let opAssignExpression = 
    parse {
        let! selectr = selector
        let! op = choice [word "+="; word "-="; word "*="; word "/="]
        let op =
            match op with 
            | "+=" -> Add
            | "-=" -> Substract
            | "*=" -> Multiply
            | "/=" -> Div
            | _ -> raise(new System.Exception("Will not happen"))
        let! value = expression
        return OperatorAssignExpression(op, selectr, value)
    }
let updateAssignmentExpression = 
    parse {
        let! selectr = selector
        let! _ = word "|="
        let! value = expression
        return UpdateAssignmentExpression(selectr, value)
    }
let assignmentExpression = 
    parse {
        let! selectr = selector
        let! _ = word "="
        let! value = expression
        return AssignmentExpression(selectr, value)
    }
expressionRef.Value <- choice([attempt assignmentExpression; attempt updateAssignmentExpression; attempt opAssignExpression; attempt selectorExpression; attempt arrayExpression; attempt objectExpression; attempt stringExpression; attempt numberExpression]) 
