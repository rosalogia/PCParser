namespace PCParser.Parser

module Operations =
    open FParsec
    open PCParser.Parser.Types
    open PCParser.Parser.Base

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