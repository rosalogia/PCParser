namespace PCParser.Parser

module Types =
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
