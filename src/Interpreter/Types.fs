namespace PCParser.Interpreter
open PCParser.Parser.Types

module Types =
    type ProgramState = {VariableTable: (Name * Value) list}