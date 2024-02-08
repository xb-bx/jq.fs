module JQ

open JSON
open FSharpx.Result

let unreachable () =
    raise (new System.Exception("Should not happen"))

let optionToResult err opt =
    match opt with
    | Some(v) -> Result.Ok(v)
    | None -> Result.Error(err)

type Selector =
    | Identifier of string * Selector
    | ArrayElement of int * Selector
    | SelectFromArray of Selector * Selector
    | Root
type Op = Add | Substract | Multiply | Div
type Expression =
    | SelectorExpression of Selector
    | AssignmentExpression of Selector * Expression
    | OperatorAssignExpression of Op * Selector * Expression
    | UpdateAssignmentExpression of Selector * Expression
    | NumberExpression of double
    | StringExpression of string
    | ObjectExpression of Map<string, Expression>
    | ArrayExpression of Expression list
    

type JQError =
    | TypeMismatch of string * string
    | PropertyNotFound of string
    | IndexOutOfBounds of int
    | InvalidOperands of JValue * JValue * Op

let typeMismatch got expected = TypeMismatch(got, expected)

let typeMismatchErr expected jv =
    match jv with
    | JObject _ -> typeMismatch "object" expected
    | JArray _ -> typeMismatch "array" expected
    | JBool _ -> typeMismatch "bool" expected
    | JString _ -> typeMismatch "string" expected
    | JNumber _ -> typeMismatch "number" expected
    | JNull -> typeMismatch "null" expected
    |> Result.Error

let listOfResults2ResultOfList (results: Result<'a, 'c> list) : Result<'a list, 'c> =
    let rec loop results acc : Result<'a list, 'c> =
        match results with
        | [] -> acc |> Result.Ok
        | Result.Ok(v) :: tail -> loop tail (acc @ [v])
        | Result.Error(v) :: _ -> Result.Error(v)

    loop results []
let mapOfResults2ResultOfMap (results: Map<'k, Result<'v, 'e>>): Result<Map<'k,'v>, 'e> =
    results |> Map.toList |> List.map (fun (k,v) -> Result.map (fun x -> (k,x)) v)
    |> listOfResults2ResultOfList |> Result.map Map.ofList





type JQValue =
    | Single of JValue
    | Many of JValue list

let rec jqToJv jq =
    match jq with
    | Single smth -> smth
    | Many smths -> smths |> JArray

let flatten jq =
    let rec flatten' jqs acc =
        match jqs with
        | [] -> acc
        | Many(jqss) :: tail -> flatten' tail (acc @ jqss)
        | Single(jq) :: tail -> flatten' tail (acc @ [jq])

    flatten' jq []

let getProp prop objk =
    match objk with
    | JObject fields -> Map.tryFind prop fields |> optionToResult (PropertyNotFound prop)
    | _ -> typeMismatchErr "object" objk

let getItem i arr =
    match arr with
    | JArray arr -> arr |> List.tryItem i |> optionToResult (IndexOutOfBounds i)
    | _ -> typeMismatchErr "array" arr

let getItemOrNull i arr =
    arr |> getItem i |> Result.toOption |> Option.defaultValue JNull

let setItem i arr value =
    match arr with
    | JArray arr ->
        if i < (arr |> List.length) then
            arr |> List.updateAt i value
        else
            let nulls = (arr |> List.length) - i
            ((List.replicate nulls JNull) @ arr) @ [value]
        |> JArray
        |> Result.Ok
    | _ -> typeMismatchErr "array" value


let getPropOrNull prop objk =
    getProp prop objk |> Result.toOption |> Option.defaultValue JNull

let setProp prop objk value =
    match objk with
    | JObject fields -> Map.add prop value fields |> JObject |> Result.Ok
    | _ -> typeMismatchErr "object" objk

let jqMap (fmap: JValue -> Result<JValue, JQError>) jq =
    match jq with
    | Single j -> fmap j |> Result.map Single
    | Many js -> List.map fmap js |> listOfResults2ResultOfList |> Result.map Many

let rec mapBySelector (fmap: JValue -> Result<JValue, JQError>) selector jv : Result<JQValue, JQError> =
    match selector with
    | Root -> jv |> jqMap fmap 
    | ArrayElement(elem, next) ->
        result {
            let! arr = evaluateSelector next jv

            let! newArr =
                match arr with
                | Single arr ->
                    fmap (getItemOrNull elem arr)
                    |> Result.bind (setItem elem arr)
                    |> Result.bind (fun newObj -> mapBySelector (fun _ -> newObj |> Result.Ok) next jv)
                | Many _ -> unreachable ()

            return newArr
        }
    | Identifier(property, next) ->
        result {
            let! obj = evaluateSelector next jv

            let! res =
                match obj with
                | Single jobj ->
                    fmap (getPropOrNull property jobj)
                    |> Result.bind (setProp property jobj)
                    |> Result.bind (fun newObj -> mapBySelector (fun _ -> newObj |> Result.Ok) next jv)
                | Many _ -> unreachable ()

            return res
        }
    | SelectFromArray(from, selec) ->
        result {
            let! arr = evaluateSelector from jv
            let arr = (jqToJv arr)

            let mapArray fmap jv =
                match jv with
                | JArray arr ->
                    arr
                    |> List.map (Single >> (mapBySelector fmap selec))
                    |> listOfResults2ResultOfList
                    |> Result.map (List.map jqToJv)
                    |> Result.map JArray
                | _ -> typeMismatchErr "array" jv

            let! newArr = mapArray fmap arr
            let! newArr = mapBySelector (fun _ -> newArr |> Result.Ok) from jv
            return newArr
        }


and evaluateSelector selector jv : Result<JQValue, JQError> =
    match selector with
    | Root -> jv |> Result.Ok
    | SelectFromArray(from, selec) ->
        result {
            let! fromval = evaluateSelector from jv

            let value =
                match fromval with
                | Single(JArray objs) ->
                    objs
                    |> List.map Single
                    |> List.map (evaluateSelector selec)
                    |> listOfResults2ResultOfList
                    |> Result.map (flatten)
                    |> Result.map Many
                | Single singleValue -> typeMismatchErr "array" singleValue
                | Many _ -> unreachable ()

            return! value
        }
    | ArrayElement(num, next) ->
        result {
            let index i elem =
                match elem with
                | JArray arr -> arr |> List.tryItem i |> optionToResult (IndexOutOfBounds i)
                | _ -> typeMismatchErr "array" elem

            let! value = evaluateSelector next jv

            let value =
                match value with
                | Single singleValue -> index num singleValue |> Result.map Single
                | Many _ -> unreachable ()

            return! value
        }
    | Identifier(property, next) ->
        result {
            let! value = evaluateSelector next jv

            let value =
                match value with
                | Single singleValue ->
                    match singleValue with
                    | JObject fields -> fields |> Map.tryFind property |> optionToResult (PropertyNotFound property)
                    | _ -> typeMismatchErr "object" singleValue
                    |> Result.map Single
                | Many _ -> unreachable ()
            return! value
        }
let add left right = 
    match (left, right) with 
    | (JNumber n1, JNumber n2) -> (JNumber (n1 + n2))  |> Result.Ok
    | (JString s1, JString s2) -> s1 + s2 |> JString |> Result.Ok
    | (JArray a1, JArray a2) -> [a1; a2] |> List.concat |> JArray |> Result.Ok
    | (JObject o1, JObject o2) -> 
        o1 |> Map.map (fun k v -> o2 |> Map.tryFind k |> Option.defaultValue v) |> JObject |> Result.Ok
    | _ -> (left, right, Add) |> InvalidOperands |> Result.Error

let sub left right = 
    match (left, right) with 
    | (JNumber n1, JNumber n2) -> (JNumber (n1 - n2))  |> Result.Ok
    | (JArray a1, JArray a2) -> a1 |> List.filter (fun v -> not (a2 |> List.contains v)) |> JArray |> Result.Ok
    | _ -> (left, right, Substract) |> InvalidOperands |> Result.Error
let apply op = 
    match op with 
    | Add -> add 
    | Substract -> sub 
    | _ -> raise (new System.NotImplementedException())


let rec evaluateExpression expression value = 
    match expression with
    | NumberExpression num -> num |> JNumber |> Single |> Result.Ok
    | StringExpression str -> str |> JString |> Single |> Result.Ok
    | SelectorExpression selectr -> 
        evaluateSelector selectr (Single value) 
    | AssignmentExpression (selectr, expression) ->
        evaluateExpression expression value
        |> Result.bind(fun x -> mapBySelector (fun _ -> x |> jqToJv |> Result.Ok) selectr (Single value))
    | UpdateAssignmentExpression (selectr, expression)  -> 
        mapBySelector (fun v -> evaluateExpression expression v |> Result.map jqToJv) selectr (Single value)
    | OperatorAssignExpression(op, selectr, expression)  -> 
        mapBySelector (fun v -> evaluateExpression expression v |> Result.bind (fun expr -> expr |> jqToJv |> apply op v )) selectr (Single value)
    | ArrayExpression exprs ->
        exprs 
        |> List.map (fun expr -> evaluateExpression expr value)
        |> listOfResults2ResultOfList
        |> Result.map (List.map jqToJv >> JArray >> Single)
    | ObjectExpression fields -> 
        fields |> Map.map (fun _ v -> evaluateExpression v value) |> mapOfResults2ResultOfMap |> Result.map (Map.map (fun _ v -> jqToJv v)) |> Result.map (JObject >> Single)

