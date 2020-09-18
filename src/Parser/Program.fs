// Learn more about F# at http://fsharp.org

open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

type Name = string

type Value =
    | Integer   of int
    | String    of string
    | Bool      of bool

type Expr =
    | Literal   of Value
    | Variable  of Name
    | Operation of Expr * string * Expr

type Statement =
    | READ      of Name
    | DISPLAY   of Expr
    | SET       of Name * Expr
    | COMPUTE   of Name * Expr
    | IF        of Expr * Block * Block option
    | WHILE     of Expr * Block
    | DOWHILE   of Block * Expr
    | REPEAT    of Expr * Block
    | HALT
and Block = Statement list

let pword s = pstring s .>> spaces

let pidentifier: Parser<string, Unit> =
    many1Satisfy2 (System.Char.IsLetter) (System.Char.IsWhiteSpace >> not)
    .>> spaces

let pread: Parser<Statement, Unit> =
    pword "READ"
    >>. pidentifier
    |>> READ

let pbool: Parser<bool, Unit> =
    pstring "true" <|> pstring "false"
    |>> System.Boolean.Parse

let pstringliteral: Parser<string, Unit> =
    let quote = pchar '\"'
    between quote quote (many anyChar)
    |>> string

let pintval: Parser<Value, Unit>    =   pint64          |>> int     |>> Integer
let pstringval: Parser<Value, Unit> =   pstringliteral  |>> String
let pboolval: Parser<Value, Unit>   =   pbool           |>> Bool

let pvalue =
    choice [
        pintval
        pstringval
        pboolval
    ]

let assumeInt f x y =
    match x with
    | Integer a ->  match y with
                    | Integer b -> Integer (f a b)

let assumeIntToBool f x y =
    match x with
    | Integer a ->  match y with
                    | Integer b -> Bool (f a b)


// This is safe, since the parser will never pass non-Integers to this function
let intOperatorParser = new OperatorPrecedenceParser<Value,Unit,Unit>()
let intexpr = intOperatorParser.ExpressionParser
let intterm = (pintval .>> spaces) <|> between (pword "(") (pword ")") intexpr
intOperatorParser.TermParser <- intterm

intOperatorParser.AddOperator(InfixOperator(">",    spaces, 1, Associativity.Left, assumeIntToBool (>)))
intOperatorParser.AddOperator(InfixOperator("<",    spaces, 1, Associativity.Left, assumeIntToBool (<)))
intOperatorParser.AddOperator(InfixOperator(">=",   spaces, 1, Associativity.Left, assumeIntToBool (>=)))
intOperatorParser.AddOperator(InfixOperator("<=",   spaces, 1, Associativity.Left, assumeIntToBool (<=)))
intOperatorParser.AddOperator(InfixOperator("==",   spaces, 1, Associativity.Left, assumeIntToBool (=)))
intOperatorParser.AddOperator(InfixOperator("!=",   spaces, 1, Associativity.Left, assumeIntToBool (<>)))
intOperatorParser.AddOperator(InfixOperator("+",    spaces, 2, Associativity.Left, assumeInt (+)))
intOperatorParser.AddOperator(InfixOperator("-",    spaces, 2, Associativity.Left, assumeInt (-)))
intOperatorParser.AddOperator(InfixOperator("*",    spaces, 3, Associativity.Left, assumeInt (*)))
intOperatorParser.AddOperator(InfixOperator("/",    spaces, 3, Associativity.Left, assumeInt (/)))
intOperatorParser.AddOperator(InfixOperator("MOD",  spaces, 3, Associativity.Left, assumeInt (%)))

let assumeBool f p q =
    match p with
    | Bool a -> match q with
                | Bool b -> Bool (f a b)

let boolOperatorParser = new OperatorPrecedenceParser<Value,Unit,Unit>()
let boolexpr = boolOperatorParser.ExpressionParser
let boolterm = (pboolval .>> spaces) <|> between (pword "(") (pword ")") boolexpr
boolOperatorParser.TermParser <- boolterm

boolOperatorParser.AddOperator(InfixOperator("AND", spaces, 1, Associativity.Left, assumeBool (&&)))
boolOperatorParser.AddOperator(InfixOperator("OR",  spaces, 1, Associativity.Left, assumeBool (||)))
boolOperatorParser.AddOperator(PrefixOperator("NOT",spaces, 1, false, function (Bool p) -> Bool (not p)))

let pvariable   = pidentifier |>> Variable
let pliteral    = pvalue |>> Literal
let poperation  = choice [intexpr ; boolexpr] |>> Literal

let rec pexpression: Parser<Expr, Unit> = choice [poperation ; pliteral ; pvariable]

let pdisplay: Parser<Statement, Unit> =
    pword "DISPLAY"
    >>. pexpression
    |>> DISPLAY

let pset: Parser<Statement, Unit> =
    let ident = between (pword "SET") (pword "AS") pidentifier

    pipe2 ident (pvariable <|> pliteral) (fun name value -> SET (name, value))

let pcompute: Parser<Statement, Unit> =
    let ident = between (pword "COMPUTE") (pword "AS") pidentifier

    pipe2 ident pexpression (fun name expression -> COMPUTE (name, expression))

let psinglestatement: Parser<Statement, Unit> =
    choice [
        pread
        pdisplay
        pset
        pcompute
    ]

let pif: Parser<Statement, Unit> =
    let parseElseOrEnd =
        (pstring "ELSE" <|> pstring "ENDIF")
        |> followedBy
        .>> spaces
    
    let parseElse =
        pword "ELSE"
        >>. manyTill (psinglestatement .>> spaces) (pword "ENDIF")

    let condition = between (pword "IF") (pword "THEN") pexpression
    let inner1 = manyTill (psinglestatement .>> spaces) parseElseOrEnd

    pipe3 condition inner1 (opt parseElse) (fun cond in1 in2 -> IF (cond, in1, in2))

let pwhile: Parser<Statement, Unit> =
    let condition =
        (pword "WHILE")
        >>. pexpression
    let inner = manyTill (psinglestatement .>> spaces) (pword "ENDIF")

    pipe2 condition inner (fun cond block -> WHILE (cond, block))
    

let pallstatements = choice [pif ; pwhile; psinglestatement]

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    argv
    |> String.concat "+"
    |> test (manyTill pallstatements (pword "HALT"))
    0
