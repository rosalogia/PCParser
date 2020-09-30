open PCParser.Parser.Types
open PCParser.Parser.Interface
open System

type ProgramState = {VariableTable: (Name * Value) list}

let lookupVariable state name =
    state.VariableTable
    |> List.filter (fun (n, _) -> n = name)
    |> List.map snd
    |> List.head

let printValue value =
    match value with
    | Integer i         -> printfn "%i" i
    | Bool b            -> printfn "%b" b
    | Value.String s    -> printfn "%s" s

let interpretConditionalValue value =
    match value with
    | Bool b    -> b
    | _         -> true

let intOperation op x y =
    match x with
    | Integer a     ->  match y with
                        | Integer b         -> Integer (op a b)
                        | Value.String b    -> Value.String (sprintf "%i%s" a b)
    | Value.String a->  match y with
                        | Integer b         -> Value.String (sprintf "%s%i" a b)
                        | Value.String b    -> Value.String (sprintf "%s%s" a b)

let add         = intOperation (+)
let subtract    = intOperation (-)
let modulus     = intOperation (%)
let multiply    = intOperation (*)
let divide      = intOperation (/)

let compare comparison x y =
    match x with
    | Integer a     ->  match y with
                        | Integer b     -> Bool (comparison a b)

let comparator f x y =
    match x with
    | Bool a    ->  match y with
                    | Bool b    -> Bool (f a b)

let andOp   = comparator (&&)
let orOp    = comparator (||)

let gt  = compare (>)
let lt  = compare (<)
let gte = compare (>=)
let lte = compare (<=)
let eq  = compare (=)
let neq = compare (<>)

let mapOperator op =
    match op with
    | ADD       -> add
    | SUBTRACT  -> subtract
    | MULTIPLY  -> multiply
    | DIVIDE    -> divide
    | MODULUS   -> modulus
    | GT        -> gt
    | LT        -> lt
    | GTE       -> gte
    | LTE       -> lte
    | EQUALS    -> eq
    | NOTEQUALS -> neq
    | AND       -> andOp
    | OR        -> orOp

let rec applyOperator state op e1 e2 =
    let reduceToValue exp =
        match exp with
        | Literal value         -> value
        | Variable name         -> lookupVariable state name
        | Operation (x, op', y) -> applyOperator state op' x y
    
    mapOperator op (reduceToValue e1) (reduceToValue e2)


let rec evaluateExpression state = function
    | Literal value -> value
    | Variable name -> lookupVariable state name
    | Operation (e1, op, e2) -> applyOperator state op e1 e2

let rec runStatement state s =
    match s with
    | DISPLAY exp ->
        printValue (evaluateExpression state exp)
        state
    | SET (name, exp) ->
        let eValue = evaluateExpression state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    | READ name ->
        let input = Console.ReadLine()
        
        let (|Int|_|) input =
           match System.Int32.TryParse(input:string) with
           | (true,int) -> Some(int)
           | _ -> None

        let (|Bool|_|) input =
           match System.Boolean.TryParse(input:string) with
           | (true,bool) -> Some(bool)
           | _ -> None
        
        let value = match input with
                    | Int i     -> Integer i
                    | Bool b    -> Bool b
                    | _         -> Value.String input
        
        {state with VariableTable = (name, value) :: state.VariableTable}
    | IF (cond, _, None) when not (cond |> evaluateExpression state |> interpretConditionalValue) -> state
    | IF (cond, _, Some (elseBlock)) when not (cond |> evaluateExpression state |> interpretConditionalValue) ->
        runStatements elseBlock state
    | IF (_, block, _)  ->
        runStatements block state
    | COMPUTE (name, exp) ->
        let eValue = evaluateExpression state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    | WHILE (cond, _) when not (cond |> evaluateExpression state |> interpretConditionalValue) -> state
    | WHILE (cond, block) ->
        let rec innerLoop innerState =
            match (cond |> evaluateExpression innerState |> interpretConditionalValue) with
            | true -> runStatements block innerState |> innerLoop
            | false -> innerState
        
        innerLoop state

and runStatements statements state =
    match statements with
    | [] -> state
    | [HALT] -> state
    | [s] ->
        runStatement state s
    | h::t ->
        runStatement state h
        |> runStatements t



let runProgram statements state =
    runStatements statements state |> ignore
    0

[<EntryPoint>]
let main argv =
    argv
    |> String.concat "+"
    |> fun path ->  match parsePCFile path with
                    | Ok result ->
                        printfn "Success: %A" result
                        runProgram result {VariableTable = []}
                    | Error err ->
                        printfn "Error: %A" err
                        1
    // |> testSingle (pchar '(' >>. pexpression .>> pchar ')')
    // |> ignore
    // 0