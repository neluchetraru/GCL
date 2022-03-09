#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

// Load dataTypes
#load "GCLTypesAST.fs"

// Open the generated lexer and parser files
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

// Function to handle the printing of an arithmetic expression
let rec printA =
    function
    | VAR x -> "VAR(" + x + ")"
    | Num x -> "Num(" + string x + ")"
    | UMinusExpr x -> "UMinusExpr(" + printA x + ")"
    | UPlusExpr x -> "UPlusExpr(" + printA x + ")"
    | Array (x, y) -> "Array(" + x + "," + printA y + ")"
    | PlusExpr (x, y) -> "PlusExpr(" + printA x + "," + printA y + ")"
    | MinusExpr (x, y) -> "MinusExpr(" + printA x + "," + printA y + ")"
    | TimesExpr (x, y) -> "TimesExpr(" + printA x + "," + printA y + ")"
    | DivExpr (x, y) -> "DivExpr(" + printA x + "," + printA y + ")"
    | PowExpr (x, y) -> "PowExpr(" + printA x + "," + printA y + ")"


// Function to handle the printing of a boolean expressions
let rec printB =
    function
    | T -> "true"
    | F -> "false"
    | UANDExpr (x, y) -> "UANDExpr(" + printB x + "," + printB y + ")"
    | UORExpr (x, y) -> "UORExpr(" + printB x + "," + printB y + ")"
    | ANDExpr (x, y) -> "ANDExpr(" + printB x + "," + printB y + ")"
    | ORExpr (x, y) -> "ORExpr(" + printB x + "," + printB y + ")"
    | NOTExpr x -> "NOTExpr(" + printB x + ")"
    | EQExpr (x, y) -> "EQExpr(" + printA x + "," + printA y + ")"
    | NEQExpr (x, y) -> "NEQExpr(" + printA x + "," + printA y + ")"
    | GTExpr (x, y) -> "GTExpr(" + printA x + "," + printA y + ")"
    | GTEExpr (x, y) -> "GTEExpr(" + printA x + "," + printA y + ")"
    | LTExpr (x, y) -> "LTExpr(" + printA x + "," + printA y + ")"
    | LTEqExpr (x, y) -> "LTEqExpr(" + printA x + "," + printA y + ")"

// Function to handle the printing of a command
let rec printCommand =
    function
    | ASSIGN (x, y) -> "ASSIGN(" + string x + "," + (printA y) + ")"
    | ArrayAT (v, x, y) ->
        "ArrayAT("
        + v
        + ","
        + (printA x)
        + ","
        + (printA y)
        + ")"
    | Skip -> "Skip"
    | Compose (x, y) ->
        "Compose("
        + (printCommand x)
        + ","
        + (printCommand y)
        + ")"
    | Do x -> "Do(" + printGCommand x + ")"
    | If x -> "If(" + printGCommand x + ")"

and printGCommand =
    function
    | ExecuteIf (x, y) ->
        "ExecuteIf("
        + (printB x)
        + ","
        + (printCommand y)
        + ")"
    | ExecuteGC (x, y) ->
        "ExecuteGC("
        + printGCommand x
        + ","
        + printGCommand y
        + ")"


// function to parse a string according to the generated parser
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    res

// Compile n programs
let rec compile n =
    match n with
    | 0 -> ()
    | n ->
        try
            let e = parse (Console.ReadLine()) // read input from CL and parse it
            printfn "%i: OK, AST: %s" (n) (printCommand (e)) // print the AST of the program - if any
            compile (n - 1)
        with
        | err ->
            printfn "%i: Compilation error." (n)
            compile (n - 1)
