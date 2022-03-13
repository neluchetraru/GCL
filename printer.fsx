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


let rec doneGC =
    function
    | ExecuteIf (x, y) -> NOTExpr(x)
    | ExecuteGC (x, y) -> ANDExpr(doneGC x, doneGC y)

// Function to handle the printing of an arithmetic expression
let rec printA =
    function
    | VAR x -> "VAR(\"" + x + "\")"
    | Num x -> "Num(" + string x + ")"
    | UMinusExpr x -> "UMinusExpr(" + printA x + ")"
    | UPlusExpr x -> "UPlusExpr(" + printA x + ")"
    | Array (x, y) -> "Array(\"" + x + "\"" + "," + printA y + ")"
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
    | ASSIGN (x, y) ->
        "ASSIGN(\""
        + string x
        + "\""
        + ","
        + (printA y)
        + ")"
    | ArrayAT (v, x, y) ->
        "ArrayAT(\""
        + v
        + "\""
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

open System.IO
// Compile n programs
let compile' () =
    Console.WriteLine("The program to parse must be in \"program.txt\" file. Press Enter to continue")

    while (Console.ReadKey().Key <> ConsoleKey.Enter) do
        ()

    let program = File.ReadAllText("program.txt")
    let e = parse (program)
    e


let compile () =
    try
        printfn "OK, AST: %s" (printCommand (compile' ()))
    with
    | err -> printfn "Compilation error."




let rec beautifyAExpr =
    function
    | VAR x -> x
    | Num x -> string x
    | UMinusExpr x -> "-" + beautifyAExpr x
    | UPlusExpr x -> "+" + beautifyAExpr x
    | Array (x, y) -> x + "[" + beautifyAExpr y + "]"
    | PlusExpr (x, y) -> beautifyAExpr x + "+" + beautifyAExpr y
    | MinusExpr (x, y) -> beautifyAExpr x + "-" + beautifyAExpr y
    | TimesExpr (x, y) -> beautifyAExpr x + "*" + beautifyAExpr y
    | DivExpr (x, y) -> beautifyAExpr x + "/" + beautifyAExpr y
    | PowExpr (x, y) -> beautifyAExpr x + "^" + beautifyAExpr y

let rec beautifyBExpr =
    function
    | T -> "true"
    | F -> "false"
    | UANDExpr (x, y) -> beautifyBExpr x + " & " + beautifyBExpr y
    | UORExpr (x, y) -> beautifyBExpr x + " | " + beautifyBExpr y
    | ANDExpr (x, y) -> beautifyBExpr x + " && " + beautifyBExpr y
    | ORExpr (x, y) -> beautifyBExpr x + " || " + beautifyBExpr y
    | NOTExpr x -> "!(" + beautifyBExpr x + ")"
    | EQExpr (x, y) -> beautifyAExpr x + " = " + beautifyAExpr y
    | NEQExpr (x, y) -> beautifyAExpr x + " <> " + beautifyAExpr y
    | GTExpr (x, y) -> beautifyAExpr x + " > " + beautifyAExpr y
    | GTEExpr (x, y) -> beautifyAExpr x + " >= " + beautifyAExpr y
    | LTExpr (x, y) -> beautifyAExpr x + " < " + beautifyAExpr y
    | LTEqExpr (x, y) -> beautifyAExpr x + " <= " + beautifyAExpr y

let rec beautifyCommand =
    function
    | ASSIGN (x, y) -> string x + ":=" + beautifyAExpr y
    | ArrayAT (x, y, z) ->
        string x
        + "["
        + beautifyAExpr y
        + "]:="
        + beautifyAExpr z
    | Skip -> "skip"



// create graph

let rec edges initial final s i =
    function
    | ASSIGN (x, y) as c -> Set.singleton ((initial, beautifyCommand c, final))
    | ArrayAT (x, y, z) as c -> Set.singleton ((initial, beautifyCommand c, final))
    | Skip as c -> Set.singleton ((initial, beautifyCommand c, final))
    | Compose (x, y) ->
        let new_q = "q" + string (i + 1)
        let E1 = edges initial new_q s (i + 1) x
        let E2 = edges new_q final s (i + 1) y
        Set.fold (fun acc el -> Set.add el acc) E2 E1
    | If x -> edges_GC initial final s (i) x
    | Do x ->
        let b = beautifyBExpr (doneGC x)
        let E = edges_GC initial initial s (i) x
        Set.add (initial, b, final) E

and edges_GC initial final s i =
    function
    | ExecuteIf (x, y) ->
        let new_q = "q" + string (i + 1)
        let E = edges new_q final s (i + 1) y
        Set.add (initial, beautifyBExpr x, new_q) E
    | ExecuteGC (x, y) ->
        let E1 = edges_GC initial final s (i) x
        let E2 = edges_GC initial final s (i + 1) y
        Set.fold (fun acc el -> Set.add el acc) E2 E1


let rec edges_d initial final s i command =
    match command with
    | ASSIGN (x, y) as c -> Set.singleton ((initial, beautifyCommand c, final))
    | ArrayAT (x, y, z) as c -> Set.singleton ((initial, beautifyCommand c, final))
    | Skip as c -> Set.singleton ((initial, beautifyCommand c, final))
    | Compose (x, y) ->
        let new_q = "q" + string (i + 1)
        let E1 = edges_d initial new_q s (i + 1) x
        let E2 = edges_d new_q final s (i + 1) y
        Set.fold (fun acc el -> Set.add el acc) (Set.fold (fun acc el -> Set.add el acc) Set.empty E1) E2
    | If x ->
        let (E, d) = edges_GC_d initial final s i x F
        E
    | Do x ->
        let (E, d) = edges_GC_d initial initial s i x F
        (Set.add (initial, beautifyBExpr (NOTExpr(d)), final) E)


and edges_GC_d initial final s i command d =
    match command with
    | ExecuteIf (x, y) ->
        let new_q = "q" + string (i + 1)
        let E = edges_d new_q final s (i + 1) y

        (Set.fold
            (fun acc el -> Set.add el acc)
            (Set.singleton ((initial, beautifyBExpr (UANDExpr(x, NOTExpr(d))), new_q)))
            E,
         (UORExpr(x, d)))
    | ExecuteGC (x, y) ->
        let (E1, d1) = edges_GC_d initial final s (i + 1) x d
        let (E2, d2) = edges_GC_d initial final s (i + 1) y d
        (Set.fold (fun acc el -> Set.add el acc) E1 E2, d2)



let get_edges c =
    function
    | false -> edges "q▷" "q◀" Set.empty 0 c
    | true -> edges_d "q▷" "q◀" Set.empty 0 c


let convert edge =

    match edge with
    | (node_in, label, node_out) ->
        node_in
        + " -> "
        + node_out
        + " [label = \""
        + label
        + "\"];\n"

let rec graphviz c =
    Console.WriteLine("[1] Non-deterministic")
    Console.WriteLine("[2] Deterministic")
    let choice = Console.ReadLine()

    File.WriteAllText(
        "graph.gv",
        "digraph program_graph {rankdir=LR;
node [shape = circle]; q▷;
node [shape = doublecircle]; q◀;
node [shape = circle]\n"
    )


    match choice with
    | "1" -> File.AppendAllText("graph.gv", Set.fold (fun acc edge -> acc + convert edge) "" (get_edges c false))
    | "2" -> File.AppendAllText("graph.gv", Set.fold (fun acc edge -> acc + convert edge) "" (get_edges c true))
    | _ ->
        File.WriteAllText("graph.gv", "")
        Console.WriteLine("Wrong option. Try again")

    if (File.ReadAllText("graph.gv") <> "") then
        File.AppendAllText("graph.gv", "}")
        printfn "PG was saved in \"graph.gv\" file."

let menu () =
    Console.WriteLine("Choose a desired option from the menu:")
    Console.WriteLine("[1] Parse a GCL program")
    Console.WriteLine("[2] Compile a GCL program into a PG")
    Console.WriteLine("[q] Quit")
    let choice = Console.ReadLine()
    choice



let rec main () =
    let option = menu ()

    match option with
    | "1" ->
        compile ()
        main ()
    | "2" ->
        try
            graphviz (compile' ())
            main ()
        with
        | err ->
            printfn "Compilation error"
            main ()
    | "q" -> ()
    | _ ->
        printfn "Please choose a correct option."
        main ()

main ()
