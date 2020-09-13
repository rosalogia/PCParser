// Learn more about F# at http://fsharp.org

open System
open FParsec

type Name = string

type Value =
    | Integer   of int
    | String    of string
    | Bool      of bool

type ArithmeticOperator =
    | ADD       of int * int
    | SUBTRACT  of int * int
    | MULTIPLY  of int * int
    | DIVIDE    of int * int
    | MOD       of int * int

type ComparisonOperator =
    | GT        of int * int
    | LT        of int * int
    | GTE       of int * int
    | LTE       of int * int

type BooleanOperator =
    | EQUALS    of Value * Value
    | NOTEQUALS of Value * Value
    | NOT       of Expr
    | AND       of Expr * Expr
    | OR        of Expr * Expr
and Expr =
    | Literal   of Value
    | Variable  of Name
    | Operation of Expr * string * Expr

type Statement =
    | READ      of Name
    | DISPLAY   of Expr
    | SET       of Name * Expr
    | COMPUTE   of Name * Expr
    | IF        of Expr * Block
    | ELSE      of Block
    | WHILE     of Expr * Block
    | DOWHILE   of Block * Expr
    | REPEAT    of Expr * Block
    | HALT
and Block = Statement list

let pidentifier: Parser<string, Unit> =
    many1Satisfy2 (System.Char.IsLetter) (System.Char.IsWhiteSpace >> not)
    .>> spaces

let pread: Parser<Statement, Unit> =
    pstring "READ"
    .>> spaces
    >>. pidentifier
    |>> READ

let pbool: Parser<bool, Unit> =
    pstring "true" <|> pstring "false"
    |>> System.Boolean.Parse

let pstringliteral: Parser<string, Unit> =
    let quote = pchar '\"'
    between quote quote (many anyChar)
    |>> string

let pexpression: Parser<Expr, Unit> =
    let pliteral =
        choice [
            pint64          |>> int     |>> Integer |>> Literal
            pstringliteral  |>> String  |>> Literal
            pbool           |>> Bool    |>> Literal
        ]
    let pvariable = pidentifier |>> Variable

    choice [pliteral ; pvariable]

let pdisplay: Parser<Statement, Unit> =
    pstring "DISPLAY"
    .>> spaces
    >>. pexpression
    |>> DISPLAY


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main _ =
    test pdisplay "DISPLAY 30"
    0
