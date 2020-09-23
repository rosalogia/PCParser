namespace PCParser

module Parser =

    open FParsec
    open System.Text

    type Name = string

    type Value =
        | Integer   of int
        | String    of string
        | Bool      of bool

    type Operator =
        | ADD
        | SUBTRACT
        | MULTIPLY
        | DIVIDE
        | MODULUS
        | GT
        | LT
        | GTE
        | LTE
        | EQUALS
        | NOTEQUALS
        | NOT
        | AND
        | OR
    type Expr =
        | Literal   of Value
        | Variable  of Name
        | Operation of Expr * Operator * Expr

    type Statement =
        | READ      of Name
        | DISPLAY   of Expr
        | SET       of Name * Expr
        | COMPUTE   of Name * Expr
        | IF        of Expr * Block * Block option
        | WHILE     of Expr * Block
        | DOWHILE   of Block * Expr
        | REPEAT    of Block * Expr
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
        let words = pchar '\"' >>. manyCharsTill anyChar (pchar '\"')
        words |>> string .>> spaces

    let pintval: Parser<Value, Unit>    =   pint64          |>> int     |>> Integer
    let pstringval: Parser<Value, Unit> =   pstringliteral  |>> String
    let pboolval: Parser<Value, Unit>   =   pbool           |>> Bool

    let pvalue =
        choice [
            pintval
            pstringval
            pboolval
        ]

    let createOperation op x y = Operation (x, op, y)

    let pvariable   = pidentifier |>> Variable
    let pliteral    = pvalue |>> Literal

    // This is safe, since the parser will never pass non-Integers to this function
    let intOperatorParser = new OperatorPrecedenceParser<Expr,Unit,Unit>()
    let intexpr = intOperatorParser.ExpressionParser
    let intterm = (pintval .>> spaces |>> Literal <|> pvariable) <|> between (pword "(") (pword ")") intexpr
    intOperatorParser.TermParser <- intterm

    intOperatorParser.AddOperator(InfixOperator(">",    spaces, 1, Associativity.Left, (createOperation GT)))
    intOperatorParser.AddOperator(InfixOperator("<",    spaces, 1, Associativity.Left, (createOperation LT)))
    intOperatorParser.AddOperator(InfixOperator(">=",   spaces, 1, Associativity.Left, (createOperation GTE)))
    intOperatorParser.AddOperator(InfixOperator("<=",   spaces, 1, Associativity.Left, (createOperation LTE)))
    intOperatorParser.AddOperator(InfixOperator("==",   spaces, 1, Associativity.Left, (createOperation EQUALS)))
    intOperatorParser.AddOperator(InfixOperator("!=",   spaces, 1, Associativity.Left, (createOperation NOTEQUALS)))
    intOperatorParser.AddOperator(InfixOperator("+",    spaces, 2, Associativity.Left, (createOperation ADD)))
    intOperatorParser.AddOperator(InfixOperator("-",    spaces, 2, Associativity.Left, (createOperation SUBTRACT)))
    intOperatorParser.AddOperator(InfixOperator("*",    spaces, 3, Associativity.Left, (createOperation MULTIPLY)))
    intOperatorParser.AddOperator(InfixOperator("/",    spaces, 3, Associativity.Left, (createOperation DIVIDE)))
    intOperatorParser.AddOperator(InfixOperator("MODULUS",  spaces, 3, Associativity.Left, (createOperation MODULUS)))

    let boolOperatorParser = new OperatorPrecedenceParser<Expr,Unit,Unit>()
    let boolexpr = boolOperatorParser.ExpressionParser
    let boolterm = (pboolval .>> spaces |>> Literal <|> intexpr) <|> between (pword "(") (pword ")") boolexpr
    boolOperatorParser.TermParser <- boolterm

    boolOperatorParser.AddOperator(InfixOperator("AND", spaces, 1, Associativity.Left, (createOperation AND)))
    boolOperatorParser.AddOperator(InfixOperator("OR",  spaces, 1, Associativity.Left, (createOperation OR)))
    // boolOperatorParser.AddOperator(PrefixOperator("NOT",spaces, 1, false, function (Bool p) -> Bool (not p)))


    let poperation  = choice [ boolexpr ; intexpr ]

    let rec pexpression: Parser<Expr, Unit> = choice [poperation ; pliteral ; pvariable]

    let pstatement, pstatementref = createParserForwardedToRef<Statement, Unit>()

    let pdisplay: Parser<Statement, Unit> =
        pword "DISPLAY"
        >>. pexpression
        |>> DISPLAY

    let pset: Parser<Statement, Unit> =
        let ident = between (pword "SET") (pword "AS") pidentifier

        let setval = pexpression

        pipe2 ident setval (fun name value -> SET (name, value))

    let pcompute: Parser<Statement, Unit> =
        let ident = between (pword "COMPUTE") (pword "AS") pidentifier

        pipe2 ident pexpression (fun name expression -> COMPUTE (name, expression))

    let pif: Parser<Statement, Unit> =
        let parseElseOrEnd =
            (pstring "ELSE" <|> pstring "ENDIF")
            |> followedBy
            .>> spaces
        
        let parseElse =
            pword "ELSE"
            >>. manyTill (pstatement .>> spaces) (pword "ENDIF")

        let condition = between (pword "IF") (pword "THEN") pexpression
        let inner1 = manyTill (pstatement .>> spaces) parseElseOrEnd

        pipe3 condition inner1 (opt parseElse) (fun cond in1 in2 -> IF (cond, in1, in2))

    let pwhile: Parser<Statement, Unit> =
        let condition =
            (pword "WHILE")
            >>. between (pstring "(") (pstring ")") pexpression
        let inner = manyTill (pstatement .>> spaces) (pword "ENDWHILE")

        pipe2 condition inner (fun cond block -> WHILE (cond, block))

    let pdowhile: Parser<Statement, Unit> =
        let inner =
            (pword "DO")
            >>. manyTill (pstatement .>> spaces) (pword "WHILE")
        
        let condition = between (pstring "(") (pstring ")") pexpression

        pipe2 inner condition (fun block cond -> DOWHILE (block, cond))

    let prepeat: Parser<Statement, Unit> =
        let inner =
            (pword "REPEAT")
            >>. manyTill (pstatement .>> spaces) (pword "UNTIL")
        
        let condition = between (pstring "(") (pstring ")") pexpression

        pipe2 inner condition (fun block cond -> REPEAT (block, cond))
    
    let phalt: Parser<Statement, Unit> =
        pword "HALT"
        >>. preturn HALT

    do pstatementref := choice [
        pread
        pdisplay
        pset
        pcompute
        pif
        pwhile
        pdowhile
        prepeat
        phalt
    ]

    let test p str =
        match runParserOnFile p () str Encoding.ASCII with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    let testSingle p str =
        match run p str with
        | Success (result, _, _) -> printfn "Success: %A" result
        | Failure (error, _, _)  -> printfn "Failure: %s" error

    let parsePCFile path =
        match runParserOnFile (many pstatement) () path Encoding.ASCII with
        | Success (result, _, _)    -> Core.Ok (result)
        | Failure (error, _, _)     -> Core.Error (error)

    (* [<EntryPoint>] *)
    (* let main argv = *)
    (*     argv *)
    (*     |> String.concat "+" *)
    (*     // |> testSingle pstatement *) 
    (*     |> test (manyTill pstatement (pword "HALT")) *)
    (*     0 *)
