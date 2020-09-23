open PCParser.Parser
open System

type ProgramState = {VariableTable: (Name * Value) list; Operations: int}

let lookupVariable state name =
    state.VariableTable
    |> List.filter (fun (n, _) -> n = name)
    |> List.map snd
    |> List.head

let incrementOps state = {state with Operations = state.Operations + 1}

let printValue value =
    match value with
    | Integer i         -> printfn "%i" i
    | Bool b            -> printfn "%b" b
    | Value.String s    -> printfn "%s" s

let interpretConditionalValue value =
    match value with
    | Bool b    -> b
    | _         -> true

let add x y =
    match x with
    | Integer a     ->  match y with
                        | Integer b         -> Integer (a + b)
                        | Value.String b    -> Value.String (sprintf "%i%s" a b)
    | Value.String a->  match y with
                        | Integer b         -> Value.String (sprintf "%s%i" a b)
                        | Value.String b    -> Value.String (sprintf "%s%s" a b)

let compare comparison x y =
    match x with
    | Integer a     ->  match y with
                        | Integer b     -> Bool (comparison a b)

let comparator f x y =
    match x with
    | Bool a    ->  match y with
                    | Bool b    -> Bool (f a b)

let andOp = comparator (&&)
let orOp = comparator (||)

let gt = compare (>)
let lt = compare (<)

let mapOperator op =
    match op with
    | ADD   -> add
    | GT    -> gt
    | LT    -> lt
    | AND   -> andOp
    | OR    -> orOp

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
        incrementOps state
    | SET (name, exp) ->
        let eValue = evaluateExpression state exp
        incrementOps {state with VariableTable = (name, eValue) :: state.VariableTable}
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
        
        incrementOps {state with VariableTable = (name, value) :: state.VariableTable}
    | IF (cond, _, None) when not (cond |> evaluateExpression state |> interpretConditionalValue) ->
        incrementOps state
    | IF (cond, _, Some (elseBlock)) when not (cond |> evaluateExpression state |> interpretConditionalValue) ->
        runStatements elseBlock (incrementOps state)
    | IF (_, block, _)  ->
        runStatements block (incrementOps state)

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
    let finalState = runStatements statements state
    printfn "Operations: %i" finalState.Operations
    0

[<EntryPoint>]
let main argv =
    argv
    |> String.concat "+"
    |> fun path ->  match parsePCFile path with
                    | Ok result ->
                        printfn "Success: %A" result
                        runProgram result {VariableTable = []; Operations = 0}
                    | Error err ->
                        printfn "Error: %A" err
                        1