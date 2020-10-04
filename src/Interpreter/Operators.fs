namespace PCParser.Interpreter
open PCParser.Parser.Types

module Operators =
    let intOperation op x y =
        match x with
        | Integer a     ->  match y with
                            | Integer b     -> Integer (op a b)

    let add         = intOperation (+)
    let subtract    = intOperation (-)
    let modulus     = intOperation (%)
    let multiply    = intOperation (*)
    let divide      = intOperation (/)

    let compare comparison x y =
        match x with
        | Integer a ->  match y with
                        | Integer b -> Bool (comparison a b)

    let junction f x y =
        match x with
        | Bool a -> match y with
                    | Bool b -> Bool (f a b)

    let conjunction = junction (&&)
    let disjunction = junction (||)

    let gt  = compare (>)
    let lt  = compare (<)
    let gte = compare (>=)
    let lte = compare (<=)
    let eq  = compare (=)
    let neq = compare (<>)

    let mapOperator = function
        | ADD       -> add
        | SUBTRACT  -> subtract
        | MULTIPLY  -> multiply
        | DIVIDE    -> divide
        | MODULUS   -> modulus
        | GT        -> gt
        | LT        -> lt
        | GTE       -> gte
        | LTE       -> lte
        | EQUALS    -> eq
        | NOTEQUALS -> neq
        | AND       -> conjunction
        | OR        -> disjunction