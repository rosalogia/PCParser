namespace PCParser.Parser

module Base =
    open FParsec
    open PCParser.Parser.Types

    let pword s = pstring s .>> spaces

    let pidentifier: Parser<string, Unit> =
        many1Satisfy2 (System.Char.IsLetter) (System.Char.IsWhiteSpace >> not)
        .>> spaces

    let pbool: Parser<bool, Unit> =
        pstring "true" <|> pstring "false"
        |>> System.Boolean.Parse

    let pstringliteral: Parser<string, Unit> =
        let words = pchar '\"' >>. manyCharsTill anyChar (pchar '\"')
        words |>> string .>> spaces



    let pintval:    Parser<Value, Unit>     =   pint64          |>> int     |>> Integer
    let pstringval: Parser<Value, Unit>     =   pstringliteral  |>> String
    let pboolval:   Parser<Value, Unit>     =   pbool           |>> Bool

    let pvalue =
        choice [
            pintval
            pstringval
            pboolval
        ]

    let createOperation op x y = Operation (x, op, y)

    let pvariable   = pidentifier |>> Variable
    let pliteral    = pvalue |>> Literal

