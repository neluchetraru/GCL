// #r "C:/fsharp/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

#load "GCLTypesAST.fs"

open GCLTypesAST
// #load "GCLParser.fs"
// open GCLParser
// #load "GCLLexer.fs"
// open GCLLexer

let rec pow (a, b) =
    match b with
    | b when b = 0 -> 1
    | b -> a * pow (a, b - 1)



let rec evalA e =
    match e with
    | Num (x) -> x
    // | Var (name) ->
    // | Array (name, x) ->
    | TimesExpr (x, y) -> evalA (x) * evalA (y)
    | DivExpr (x, y) -> evalA (x) / evalA (y)
    | PlusExpr (x, y) -> evalA (x) + evalA (y)
    | MinusExpr (x, y) -> evalA (x) - evalA (y)
    | PowExpr (x, y) -> pow (evalA (x), evalA (y))
    | UPlusExpr (x) -> evalA (x)
    | UMinusExpr (x) -> - evalA(x)


let rec evalB e =
    match e with
    | T -> true
    | F -> false
    | UAnd (x, y) ->
        if (evalB (x)) then
            (evalB (x) && evalB (y))
        else
            false
    | UOr (x, y) ->
        if (evalB (x)) then
            true
        else
            (evalB (x) || evalB (y))
    | And (x, y) -> evalB (x) && evalB (y)
    | Or (x, y) -> evalB (x) || evalB (y)
    | Not (x) -> not (evalB (x))
    | Eq (x, y) -> evalA (x) = evalA (y)
    | NEq (x, y) -> evalA (x) <> evalA (y)
    | GT (x, y) -> evalA (x) > evalA (y)
    | GTEq (x, y) -> evalA (x) >= evalA (y)
    | LT (x, y) -> evalA (x) < evalA (y)
    | LTEq (x, y) -> evalA (x) <= evalA (y)
// let rec executeC c
// let rec executeGC gc
