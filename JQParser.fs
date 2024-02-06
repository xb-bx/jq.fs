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
let identifierWithIndexer previous = 
    parse {
        let! ident = identifier (IdentifierOptions()) 
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
        let! _ = pchar '.'
        let current = Root
        let! parsed = sepBy (choice [ identifierWithIndexer current; selectFromArray current; arrayElement current;  ]) (pchar '.')
        return (parsed |> List.fold setFrom current)
        (*return current*)
    }


