open PCParser.Parser.Interface
open PCParser.Interpreter.Types
open PCParser.Interpreter.Statements

let runProgram statements state =
    runStatements statements state |> ignore
    0

[<EntryPoint>]
let main argv =
    argv
    |> String.concat "+"
    |> fun path ->  match parsePCFile path with
                    | Ok result ->
                        runProgram result {VariableTable = []}
                    | Error err ->
                        printfn "Error: %A" err
                        1