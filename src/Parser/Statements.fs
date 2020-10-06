namespace PCParser.Parser

module Statements =
    open FParsec
    open PCParser.Parser.Types
    open PCParser.Parser.Base
    open PCParser.Parser.Operations

    let pstatement, pstatementref = createParserForwardedToRef<Statement, Unit>()

    let pread: Parser<Statement, Unit> =
        pword "READ"
        >>. pidentifier
        |>> READ
    
    let pdisplay: Parser<Statement, Unit> =
        pword "DISPLAY"
        >>. pexpression
        |>> DISPLAY

    let pset: Parser<Statement, Unit> =
        let ident = between (pword "SET") (pword "TO") pidentifier

        let setval = pexpression

        pipe2 ident setval (fun name value -> SET (name, value))

    let pcompute: Parser<Statement, Unit> =
        let ident = between (pword "COMPUTE") (pword "AS") pidentifier

        pipe2 ident pexpression (fun name expression -> COMPUTE (name, expression))

    let pif: Parser<Statement, Unit> =
        let parseElseOrEnd =
            (pword "ELSE" <|> pword "ENDIF")
        
        let parseElse =
            pword "ELSE"
            >>. many (notFollowedBy parseElseOrEnd >>. pstatement .>> spaces)

        let condition = between (pword "IF") (pword "THEN") pexpression
        let inner1 = many (notFollowedBy parseElseOrEnd >>. pstatement .>> spaces) 
        let elseOption = opt parseElse
        
        pipe3 condition inner1 elseOption (fun cond in1 in2 -> IF (cond, in1, in2)) .>> pword "ENDIF"



    let pwhile: Parser<Statement, Unit> =
        let condition =
            (pword "WHILE")
            >>. between (pword "(") (pword ")") pexpression
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
        pwhile
        pdowhile
        prepeat
        pif
        phalt
    ]
