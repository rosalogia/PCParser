namespace PCParser.Interpreter
open PCParser.Parser.Types
open PCParser.Interpreter.Types
open PCParser.Interpreter.Expressions
open System

module Statements =
    let display exp state =
        printValue (evaluateExpression state exp)
        state

    let set name exp state =
        let eValue = evaluateExpression state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    let read name state =
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

    let compute name exp state =
        let eValue = evaluateExpression state exp
        {state with VariableTable = (name, eValue) :: state.VariableTable}
    
    let rec conditional cond block elseBlockOption state =
        if (evaluateCondition cond state) then
            runStatements block state
        else
            match elseBlockOption with
            | Some (elseBlock)  -> runStatements elseBlock state
            | None              -> state

    and whileloop cond block state =
        if (evaluateCondition cond state) then
            let rec innerLoop innerState =
                if (cond |> evaluateExpression innerState |> interpretConditionalValue) then 
                    runStatements block innerState |> innerLoop
                else
                    innerState
                
            innerLoop state
        else
            state
    
    and dowhile cond block state =
        let updatedState = runStatements block state
        
        if  (evaluateCondition cond updatedState) then
            let rec innerLoop innerState =
                if (evaluateCondition cond innerState) then 
                    runStatements block innerState
                    |> innerLoop
                else
                    innerState
                
            innerLoop updatedState 
        else
            updatedState
    
    and repeat n block state =
        let n' = interpretIntegerValue n
        let rec loop acc state' =
            match (acc, state') with
            | (count, s) when count = n'    -> runStatements block s
            | (_, s)                        -> runStatements block s |> loop (acc + 1)
        
        loop 0 state

    and runStatement state s =
        state
        |>  match s with
            | DISPLAY exp                           -> display exp
            | SET (name, exp)                       -> set name exp
            | READ name                             -> read name
            | IF (cond, block, elseBlockOption)     -> conditional cond block elseBlockOption
            | COMPUTE (name, exp)                   -> compute name exp
            | WHILE (cond, block)                   -> whileloop cond block
            | DOWHILE (block, cond)                 -> dowhile cond block
            | REPEAT (block, count)                 -> repeat (evaluateExpression state count) block
            | HALT                                  -> id

    and runStatements statements state =
        match statements with
        | []        ->  state
        | [HALT]    ->  state
        | [s]       ->  runStatement state s
        | h::t      ->  runStatement state h
                        |> runStatements t
