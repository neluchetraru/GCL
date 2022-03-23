module Utils

let rec pow (a, b) =
    match b with
    | b when b = 0 -> 1
    | b -> a * pow (a, b - 1)


let convert edge =
    match edge with
    | (node_in, label, node_out, act: Act) ->
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


//
type Act =
    | B of bexpr
    | C of command



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


let rec semA e =
    match e with
    | Num (x) -> x
    // | VAR (name) -> 1
    // | Array (name, x) ->
    | TimesExpr (x, y) -> semA (x) * semA (y)
    | DivExpr (x, y) -> semA (x) / semA (y)
    | PlusExpr (x, y) -> semA (x) + semA (y)
    | MinusExpr (x, y) -> semA (x) - semA (y)
    | PowExpr (x, y) -> pow (semA (x), semA (y))
    | UPlusExpr (x) -> semA (x)
    | UMinusExpr (x) -> - semA(x)

let rec semB e =
    match e with
    | T -> true
    | F -> false
    | UANDExpr (x, y) ->
        if (semB (x)) then
            (semB (x) && semB (y))
        else
            false
    | UORExpr (x, y) ->
        if (semB (x)) then
            true
        else
            (semB (x) || semB (y))
    | ANDExpr (x, y) -> semB (x) && semB (y)
    | ORExpr (x, y) -> semB (x) || semB (y)
    | NOTExpr (x) -> not (semB (x))
    | EQExpr (x, y) -> semA (x) = semA (y)
    | NEQExpr (x, y) -> semA (x) <> semA (y)
    | GTExpr (x, y) -> semA (x) > semA (y)
    | GTEExpr (x, y) -> semA (x) >= semA (y)
    | LTExpr (x, y) -> semA (x) < semA (y)
    | LTEqExpr (x, y) -> semA (x) <= semA (y)
