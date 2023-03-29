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
// #load "MemoryParser.fs"
// open MemoryParser
// #load "MemoryLexer.fs"
// open MemoryLexer

type Act =
    | B of bexpr
    | C of command


// function to parse a string according to the generated parser
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    res

// let parseMem input =
//     let lexbuf = LexBuffer<char>.FromString input

//     let res =
//         MemoryParser.start MemoryLexer.tokenize lexbuf

//     res

let rec pow (a, b) =
    match b with
    | b when b = 0 -> 1
    | b -> a * pow (a, b - 1)


let convert edge =
    match edge with
    | (node_in, label, node_out, act, l) ->
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


type loop = Loop of bool

let rec edges initial final i =
    function
    | Assign (x, y) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c, Loop(false))), i)
    | AssignAt (x, y, z) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c, Loop(false))), i)
    | Skip as c -> (Set.singleton ((initial, beautifyCommand c, final, C c, Loop(false))), i)
    | Compose (x, y) ->
        let new_q = "q" + string (i)
        let E1, last = edges initial new_q (i + 1) x
        let E2, last = edges new_q final (last) y
        ((Set.fold (fun acc el -> Set.add el acc) E2 E1), last)

    | If x -> edges_GC initial final (i) x
    | Do x ->
        let b = beautifyBExpr (doneGC x)
        let E, last = edges_GC initial initial (i) x
        (Set.add (initial, b, final, B(doneGC x), Loop(true)) E, last)

and edges_GC initial final i =
    function
    | ExecuteGC (x, y) ->
        let E1, last = edges_GC initial final (i) x
        let E2, last = edges_GC initial final (last) y
        (Set.fold (fun acc el -> Set.add el acc) E2 E1, last)
    | ExecuteIf (x, y) ->
        let new_q = "q" + string (i)
        let E, last = edges new_q final (i + 1) y
        (Set.add (initial, beautifyBExpr x, new_q, B x, Loop(false)) E, last)



let rec edges_d initial final i command =
    match command with
    | Assign (x, y) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c, Loop(false))), i)
    | AssignAt (x, y, z) as c -> (Set.singleton ((initial, beautifyCommand c, final, C c, Loop(false))), i)
    | Skip as c -> (Set.singleton ((initial, beautifyCommand c, final, C c, Loop(false))), i)
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
        (Set.add (initial, beautifyBExpr (NOTExpr(d)), final, B(NOTExpr(d)), Loop(true)) E, last)


and edges_GC_d initial final i command d =
    match command with
    | ExecuteIf (x, y) ->
        let new_q = "q" + string (i)
        let (E, last) = edges_d new_q final (i + 1) y

        (Set.fold
            (fun acc el -> Set.add el acc)
            (Set.singleton (
                (initial, beautifyBExpr (UANDExpr(x, NOTExpr(d))), new_q, B((UANDExpr(x, NOTExpr(d)))), Loop(false))
            ))
            E,
         (UORExpr(x, d)),
         last)
    | ExecuteGC (x, y) ->
        let (E1, d1, last) = edges_GC_d initial final (i) x d
        let (E2, d2, last) = edges_GC_d initial final (last) y d1
        (Set.fold (fun acc el -> Set.add el acc) E1 E2, d2, last)




// Semantics
let stack_var =
    Map
        .empty
        .Add("i", 0)
        .Add("n", 10)
        .Add("t", 0)
        .Add("j", 0)

let stack_list = Map.empty.Add("A", [ 54; 32; 1 ])
// "1,32,43" -> [1,32,43]

type State =
    | Stuck of string
    | Success of string

let rec semA e (stack_var: Map<string, int>) (stack_list: Map<string, int list>) =
    match e with
    | Num (x) -> x
    | VAR (name) ->
        match stack_var.TryFind name with
        | Some x -> x
        | None -> failwith " "
    | Array (name, x) ->
        match stack_list.TryFind name with
        | Some arr ->
            try
                arr.[semA x stack_var stack_list]
            with
            | e -> failwith "Stuck: index out of bounds"
        | None -> failwith "Stuck: list not defined"
    | TimesExpr (x, y) ->
        semA (x) stack_var stack_list
        * semA (y) stack_var stack_list
    | DivExpr (x, y) ->
        semA (x) stack_var stack_list
        / semA (y) stack_var stack_list
    | PlusExpr (x, y) ->
        semA (x) stack_var stack_list
        + semA (y) stack_var stack_list
    | MinusExpr (x, y) ->
        semA (x) stack_var stack_list
        - semA (y) stack_var stack_list
    | PowExpr (x, y) -> pow (semA (x) stack_var stack_list, semA (y) stack_var stack_list)
    | UPlusExpr (x) -> semA (x) stack_var stack_list
    | UMinusExpr (x) -> -semA (x) stack_var stack_list

let rec semB e (stack_var: Map<string, int>) (stack_list: Map<string, int list>) =
    match e with
    | T -> true
    | F -> false
    | UANDExpr (x, y) ->
        if (semB (x) stack_var stack_list) then
            (semB (x) stack_var stack_list
             && semB (y) stack_var stack_list)
        else
            false
    | UORExpr (x, y) ->
        if (semB (x) stack_var stack_list) then
            true
        else
            (semB (x) stack_var stack_list
             || semB (y) stack_var stack_list)
    | ANDExpr (x, y) ->
        semB (x) stack_var stack_list
        && semB (y) stack_var stack_list
    | ORExpr (x, y) ->
        semB (x) stack_var stack_list
        || semB (y) stack_var stack_list
    | NOTExpr (x) -> not (semB (x) stack_var stack_list)
    | EQExpr (x, y) -> semA (x) stack_var stack_list = semA (y) stack_var stack_list
    | NEQExpr (x, y) ->
        semA (x) stack_var stack_list
        <> semA (y) stack_var stack_list
    | GTExpr (x, y) -> semA (x) stack_var stack_list > semA (y) stack_var stack_list
    | GTEExpr (x, y) ->
        semA (x) stack_var stack_list
        >= semA (y) stack_var stack_list
    | LTExpr (x, y) -> semA (x) stack_var stack_list < semA (y) stack_var stack_list
    | LTEqExpr (x, y) ->
        semA (x) stack_var stack_list
        <= semA (y) stack_var stack_list


let Sem act stack_var stack_list =
    match act with
    | B x ->
        if (semB x stack_var stack_list) then
            (stack_var, stack_list)
        else
            failwith "stuck"
    | C x ->
        match x with
        | Assign (x, y) ->
            match stack_var.TryFind(x) with
            | Some var -> (stack_var.Add(x, semA y stack_var stack_list), stack_list)
            | None -> failwith "Variable not defined"
        | AssignAt (x, y, z) ->
            match stack_list.TryFind(x) with
            | Some arr ->
                try
                    let indices = [ 0 .. (List.length arr - 1) ]

                    let t =
                        List.fold
                            (fun acc (el, index) ->
                                if index = (semA y stack_var stack_list) then
                                    (semA z stack_var stack_list :: acc)
                                else
                                    el :: acc)
                            []
                            (List.zip arr indices)

                    (stack_var, stack_list.Add(x, t))
                with
                | e -> failwith "Array  index out of bounds"
            | None -> failwith "Vairable not defined"

//  AssignAt

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



let rec iterate PG node (stack_var, stack_list) =
    if node = "q◀" then
        (stack_var, stack_list, Success node)
    else
        let available =
            Set.fold
                (fun acc el ->
                    match el with
                    | (q0, _, qf, act, l) -> if q0 = node then el :: acc else acc)
                []
                PG

        let res =
            Set.toList (
                List.fold
                    (fun acc el ->
                        match el with
                        | (q0, _, qf, act, l) ->
                            match act with
                            | B x ->
                                if (semB x stack_var stack_list = true) then
                                    Set.add el acc
                                else
                                    acc
                            | C x -> Set.add el acc)
                    Set.empty
                    available
            )



        if (List.length res = 1) then // Deterministic
            let (_, _, qf, act, l) = res.[0]

            try
                iterate PG qf (Sem act stack_var stack_list)
            with
            | e -> (stack_var, stack_list, Stuck qf)
        else
            (stack_var, stack_list, Stuck node)


let printMemVar (stack_var: Map<string, int>) =
    Map.iter (fun k v -> printf "%s: %i\n" k v) stack_var
// printfn t

let printMemList (stack_list: Map<string, int list>) =
    Map.iter (fun k v -> printfn "%s: %s\n" k (List.fold (fun acc el -> acc + " " + (string el)) "" v)) stack_list

let execute () =
    let (stack_var, stack_list, state) =
        iterate (get_edges (compile' ()) false) "q▷" (stack_var, stack_list)

    match state with
    | Success node ->
        printfn "Status: Terminated"
        printfn "Node: %s" (node)
        printMemVar (stack_var)
        printMemList (stack_list)

    | Stuck node ->
        printfn "Status: Stuck"
        printfn "Node: %s" (node)
        printMemVar (stack_var)
        printMemList (stack_list)


let rec get_coverage edges res =
    match edges with
    | [] -> res
    | (q0, _, _, _, s) :: xs ->
        match s with
        | Loop true -> get_coverage xs (Set.add q0 res)
        | Loop false -> get_coverage xs res


let EDGES_END_IN_INIT init edges =
    Set.fold
        (fun acc ((_, _, qf, _, _) as edge) ->
            if qf = init then
                Set.add edge acc
            else
                acc)
        Set.empty
        edges


let rec build init dict final edges cov =
    let edges_end_in_init =
        Set.toList (EDGES_END_IN_INIT init edges)

    build_help init dict final cov edges edges_end_in_init

and build_help init dict final cov edges =
    function
    | (q, act, qf, _, _) :: tail ->


        if Set.exists (fun x -> x = q) cov then
            Set.add (q, act + " " + dict, final) (build_help init dict final cov edges tail)
        else
            Set.fold
                (fun acc el -> Set.add el acc)
                (build_help init dict final cov edges tail)
                (build q (act + " " + dict) final edges cov)
    | [] -> Set.empty

let rec spf coverage edges =
    let S =
        Set.fold (fun acc x -> Set.union (build x "" x edges coverage) acc) Set.empty coverage

    S

let beautifySPF spf =
    Set.fold (fun acc (q0, act, qf) -> acc + (q0 + " " + act + " " + qf + "\n")) "" spf


let menu () =
    Console.WriteLine("Choose a desired option from the menu:")
    Console.WriteLine("[1] Parse a GCL program")
    Console.WriteLine("[2] Compile a GCL program into a PG")
    Console.WriteLine("[3] Execute a program")
    Console.WriteLine("[4] Program verification")
    Console.WriteLine("[q] Quit")
    let choice = Console.ReadLine()
    choice

let rec programVerif c =
    Console.WriteLine("Choose a option:")
    Console.WriteLine("[1] Get Covering Nodes")
    Console.WriteLine("[2] Partial Predicate Assignment")
    Console.WriteLine("[3] Show Proof Obligations")
    Console.WriteLine("[4] Go back")

    let cov =
        (get_coverage (Set.toList (get_edges c false)) (Set.empty.Add("q▷")))
            .Add("q◀")

    let choice = Console.ReadLine()

    match choice with
    | "1" ->
        Console.WriteLine(Set.fold (fun acc x -> acc + x + "\n") "" cov)
        programVerif c
    | "2" ->
        Console.WriteLine(beautifySPF (spf cov (get_edges c false)))
        programVerif c
    | "3" ->
        Console.WriteLine(beautifySPF (spf cov (get_edges c false)))
        programVerif c
    | "4" -> ()
    | _ -> programVerif c

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
    | "3" ->
        execute ()
        main ()

    | "4" ->
        let c = compile' ()
        programVerif c
        main ()
    | "q" -> ()
    | _ ->
        printfn "Please choose a correct option."
        main ()

main ()


//q(start, final) and loops are covering nodes
