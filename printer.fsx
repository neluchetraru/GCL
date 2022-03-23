#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System.IO

open System

#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

type Act =
    | B of bexpr
    | C of command


// function to parse a string according to the generated parser
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    res

let rec pow (a, b) =
    match b with
    | b when b = 0 -> 1
    | b -> a * pow (a, b - 1)


let convert edge =
    match edge with
    | (node_in, label, node_out, act) ->
        node_in
        + " -> "
        + node_out
        + " [label = \""
        + label
        + "\"];\n"

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
    | Assign (x, y) ->
        "Assign(\""
        + string x
        + "\""
        + ","
        + (printA y)
        + ")"
    | AssignAt (v, x, y) ->
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




//  BEAUTIFY

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
    | Assign (x, y) -> string x + ":=" + beautifyAExpr y
    | AssignAt (x, y, z) ->
        string x
        + "["
        + beautifyAExpr y
        + "]:="
        + beautifyAExpr z
    | Skip -> "skip"
    | _ -> failwith "error"


//



// EDGES

// create graph

let rec edges initial final i =
    function
    | Assign (x, y) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c)), i)
    | AssignAt (x, y, z) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c)), i)
    | Skip as c -> (Set.singleton ((initial, beautifyCommand c, final, C c)), i)
    | Compose (x, y) ->
        let new_q = "q" + string (i)
        let E1, last = edges initial new_q (i + 1) x
        let E2, last = edges new_q final (last) y
        ((Set.fold (fun acc el -> Set.add el acc) E2 E1), last)

    | If x -> edges_GC initial final (i) x
    | Do x ->
        let b = beautifyBExpr (doneGC x)
        let E, last = edges_GC initial initial (i) x
        (Set.add (initial, b, final, B(doneGC x)) E, last)

and edges_GC initial final i =
    function
    | ExecuteGC (x, y) ->
        let E1, last = edges_GC initial final (i) x
        let E2, last = edges_GC initial final (last) y
        (Set.fold (fun acc el -> Set.add el acc) E2 E1, last)
    | ExecuteIf (x, y) ->
        let new_q = "q" + string (i)
        let E, last = edges new_q final (i + 1) y
        (Set.add (initial, beautifyBExpr x, new_q, B x) E, last)



let rec edges_d initial final i command =
    match command with
    | Assign (x, y) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c)), i)
    | AssignAt (x, y, z) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c)), i)
    | Skip as c -> (Set.singleton ((initial, beautifyCommand c, final, C c)), i)
    | Compose (x, y) ->
        let new_q = "q" + string (i)
        let E1, last = edges_d initial new_q (i + 1) x
        let E2, last = edges_d new_q final (last) y
        (Set.fold (fun acc el -> Set.add el acc) (Set.fold (fun acc el -> Set.add el acc) Set.empty E1) E2, last)
    | If x ->
        let (E, d, last) = edges_GC_d initial final i x F
        (E, last)
    | Do x ->
        let (E, d, last) = edges_GC_d initial initial i x F
        (Set.add (initial, beautifyBExpr (NOTExpr(d)), final, B(NOTExpr(d))) E, last)


and edges_GC_d initial final i command d =
    match command with
    | ExecuteIf (x, y) ->
        let new_q = "q" + string (i)
        let (E, last) = edges_d new_q final (i + 1) y

        (Set.fold
            (fun acc el -> Set.add el acc)
            (Set.singleton ((initial, beautifyBExpr (UANDExpr(x, NOTExpr(d))), new_q, B((UANDExpr(x, NOTExpr(d)))))))
            E,
         (UORExpr(x, d)),
         last)
    | ExecuteGC (x, y) ->
        let (E1, d1, last) = edges_GC_d initial final (i) x d
        let (E2, d2, last) = edges_GC_d initial final (last) y d1
        (Set.fold (fun acc el -> Set.add el acc) E1 E2, d2, last)




// Semantics
let stack = Map.empty

let rec semA e (stack: Map<string, int>) =
    match e with
    | Num (x) -> x
    | VAR (name) ->
        match stack.TryFind name with
        | Some x -> x
        | None -> failwith " "
    // | Array (name, x) ->
    | TimesExpr (x, y) -> semA (x) stack * semA (y) stack
    | DivExpr (x, y) -> semA (x) stack / semA (y) stack
    | PlusExpr (x, y) -> semA (x) stack + semA (y) stack
    | MinusExpr (x, y) -> semA (x) stack - semA (y) stack
    | PowExpr (x, y) -> pow (semA (x) stack, semA (y) stack)
    | UPlusExpr (x) -> semA (x) stack
    | UMinusExpr (x) -> -semA (x) stack

let rec semB e (stack: Map<string, int>) =
    match e with
    | T -> true
    | F -> false
    | UANDExpr (x, y) ->
        if (semB (x) stack) then
            (semB (x) stack && semB (y) stack)
        else
            false
    | UORExpr (x, y) ->
        if (semB (x) stack) then
            true
        else
            (semB (x) stack || semB (y) stack)
    | ANDExpr (x, y) -> semB (x) stack && semB (y) stack
    | ORExpr (x, y) -> semB (x) stack || semB (y) stack
    | NOTExpr (x) -> not (semB (x) stack)
    | EQExpr (x, y) -> semA (x) stack = semA (y) stack
    | NEQExpr (x, y) -> semA (x) stack <> semA (y) stack
    | GTExpr (x, y) -> semA (x) stack > semA (y) stack
    | GTEExpr (x, y) -> semA (x) stack >= semA (y) stack
    | LTExpr (x, y) -> semA (x) stack < semA (y) stack
    | LTEqExpr (x, y) -> semA (x) stack <= semA (y) stack


let Sem act stack =
    match act with
    | B x ->
        if (semB x stack) then
            stack
        else
            failwith "stuck"
    | C x ->
        match x with
        | Assign (x, y) -> stack.Add(x, semA y stack)
//
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


let get_edges (c: command) =
    function
    | true ->
        let E, last = edges_d "q▷" "q◀" 1 c
        E
    | false ->
        let E, last = edges "q▷" "q◀" 1 c
        E

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



let rec iterate PG node stack =
    if node = "q◀" then
        stack
    else
        let available =
            Set.fold
                (fun acc el ->
                    match el with
                    | (q0, _, qf, act) -> if q0 = node then el :: acc else acc)
                []
                PG

        if (List.length available = 1) then // Deterministic
            let (_, _, qf, act) = available.[0]

            iterate PG qf (Sem act stack)
        else
            failwith "Non-determinism"

// let res =
//     Set.toList (
//         Set.fold
//             (fun acc el ->
//                 match el with
//                 | (q0, _, qf, act) ->
//                     match act with
//                     | B x ->
//                         if (semB x = true) then
//                             Set.add el acc
//                         else
//                             acc
//                     | Assign _ -> Set.add el acc
//                     | SKIP -> Set.add el acc
//                     | AssignArray _ -> Set.add el acc)
//             Set.empty
//             available
//     )


// let qf =
//     match res with
//     | (_, _, q, _) :: [] -> q
//     | (_, _, q, _) :: xs -> failwith "err"

// ()

// // q▷


let menu () =
    Console.WriteLine("Choose a desired option from the menu:")
    Console.WriteLine("[1] Parse a GCL program")
    Console.WriteLine("[2] Compile a GCL program into a PG")
    Console.WriteLine("[3] Execute a program")
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
    // | "3" -> execute (get_edges (compile' ()) false) "q▷" stack // CHANGFE
    | "q" -> ()
    | _ ->
        printfn "Please choose a correct option."
        main ()

main ()
