namespace PCParser.Parser

module Interface =
    open FParsec
    open PCParser.Parser.Statements
    open System.Text

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