module GCLTypesAST

type aexpr =
    | VAR of string
    | Num of int
    | UMinusExpr of (aexpr)
    | UPlusExpr of (aexpr)
    | Array of (string * aexpr)
    | PlusExpr of (aexpr * aexpr)
    | MinusExpr of (aexpr * aexpr)
    | TimesExpr of (aexpr * aexpr)
    | DivExpr of (aexpr * aexpr)
    | PowExpr of (aexpr * aexpr)


type bexpr =
    | T
    | F
    | UANDExpr of (bexpr * bexpr)
    | UORExpr of (bexpr * bexpr)
    | ANDExpr of (bexpr * bexpr)
    | ORExpr of (bexpr * bexpr)
    | NOTExpr of bexpr
    | EQExpr of (aexpr * aexpr)
    | NEQExpr of (aexpr * aexpr)
    | GTExpr of (aexpr * aexpr)
    | GTEExpr of (aexpr * aexpr)
    | LTExpr of (aexpr * aexpr)
    | LTEqExpr of (aexpr * aexpr)


type command =
    | ASSIGN of (string * aexpr)
    | ArrayAT of (string * aexpr * aexpr)
    | Skip
    | Compose of (command * command)
    | If of gCommand
    | Do of gCommand

and gCommand =
    | ExecuteIf of (bexpr * command)
    | ExecuteGC of (gCommand * gCommand)
