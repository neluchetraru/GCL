// Implementation file for parser generated by fsyacc
module GCLParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "GCLParser.fsp"

open GCLTypesAST

# 10 "GCLParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | ASSGN
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | SRB
  | SLB
  | SEMICOLON
  | TT
  | FF
  | UAND
  | UOR
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | GT
  | GTE
  | LT
  | LTE
  | SKIP
  | IF
  | DO
  | ENDDO
  | ENDIF
  | ARROW
  | COMPOSE
  | EOF
  | VARNAME of (string)
  | NUM of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_ASSGN
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_SRB
    | TOKEN_SLB
    | TOKEN_SEMICOLON
    | TOKEN_TT
    | TOKEN_FF
    | TOKEN_UAND
    | TOKEN_UOR
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_NOT
    | TOKEN_EQ
    | TOKEN_NEQ
    | TOKEN_GT
    | TOKEN_GTE
    | TOKEN_LT
    | TOKEN_LTE
    | TOKEN_SKIP
    | TOKEN_IF
    | TOKEN_DO
    | TOKEN_ENDDO
    | TOKEN_ENDIF
    | TOKEN_ARROW
    | TOKEN_COMPOSE
    | TOKEN_EOF
    | TOKEN_VARNAME
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_a
    | NONTERM_b
    | NONTERM_C
    | NONTERM_GC

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | ASSGN  -> 0 
  | TIMES  -> 1 
  | DIV  -> 2 
  | PLUS  -> 3 
  | MINUS  -> 4 
  | POW  -> 5 
  | LPAR  -> 6 
  | RPAR  -> 7 
  | SRB  -> 8 
  | SLB  -> 9 
  | SEMICOLON  -> 10 
  | TT  -> 11 
  | FF  -> 12 
  | UAND  -> 13 
  | UOR  -> 14 
  | AND  -> 15 
  | OR  -> 16 
  | NOT  -> 17 
  | EQ  -> 18 
  | NEQ  -> 19 
  | GT  -> 20 
  | GTE  -> 21 
  | LT  -> 22 
  | LTE  -> 23 
  | SKIP  -> 24 
  | IF  -> 25 
  | DO  -> 26 
  | ENDDO  -> 27 
  | ENDIF  -> 28 
  | ARROW  -> 29 
  | COMPOSE  -> 30 
  | EOF  -> 31 
  | VARNAME _ -> 32 
  | NUM _ -> 33 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_ASSGN 
  | 1 -> TOKEN_TIMES 
  | 2 -> TOKEN_DIV 
  | 3 -> TOKEN_PLUS 
  | 4 -> TOKEN_MINUS 
  | 5 -> TOKEN_POW 
  | 6 -> TOKEN_LPAR 
  | 7 -> TOKEN_RPAR 
  | 8 -> TOKEN_SRB 
  | 9 -> TOKEN_SLB 
  | 10 -> TOKEN_SEMICOLON 
  | 11 -> TOKEN_TT 
  | 12 -> TOKEN_FF 
  | 13 -> TOKEN_UAND 
  | 14 -> TOKEN_UOR 
  | 15 -> TOKEN_AND 
  | 16 -> TOKEN_OR 
  | 17 -> TOKEN_NOT 
  | 18 -> TOKEN_EQ 
  | 19 -> TOKEN_NEQ 
  | 20 -> TOKEN_GT 
  | 21 -> TOKEN_GTE 
  | 22 -> TOKEN_LT 
  | 23 -> TOKEN_LTE 
  | 24 -> TOKEN_SKIP 
  | 25 -> TOKEN_IF 
  | 26 -> TOKEN_DO 
  | 27 -> TOKEN_ENDDO 
  | 28 -> TOKEN_ENDIF 
  | 29 -> TOKEN_ARROW 
  | 30 -> TOKEN_COMPOSE 
  | 31 -> TOKEN_EOF 
  | 32 -> TOKEN_VARNAME 
  | 33 -> TOKEN_NUM 
  | 36 -> TOKEN_end_of_input
  | 34 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_a 
    | 3 -> NONTERM_a 
    | 4 -> NONTERM_a 
    | 5 -> NONTERM_a 
    | 6 -> NONTERM_a 
    | 7 -> NONTERM_a 
    | 8 -> NONTERM_a 
    | 9 -> NONTERM_a 
    | 10 -> NONTERM_a 
    | 11 -> NONTERM_a 
    | 12 -> NONTERM_a 
    | 13 -> NONTERM_b 
    | 14 -> NONTERM_b 
    | 15 -> NONTERM_b 
    | 16 -> NONTERM_b 
    | 17 -> NONTERM_b 
    | 18 -> NONTERM_b 
    | 19 -> NONTERM_b 
    | 20 -> NONTERM_b 
    | 21 -> NONTERM_b 
    | 22 -> NONTERM_b 
    | 23 -> NONTERM_b 
    | 24 -> NONTERM_b 
    | 25 -> NONTERM_b 
    | 26 -> NONTERM_b 
    | 27 -> NONTERM_C 
    | 28 -> NONTERM_C 
    | 29 -> NONTERM_C 
    | 30 -> NONTERM_C 
    | 31 -> NONTERM_C 
    | 32 -> NONTERM_C 
    | 33 -> NONTERM_GC 
    | 34 -> NONTERM_GC 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 36 
let _fsyacc_tagOfErrorTerminal = 34

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | ASSGN  -> "ASSGN" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | POW  -> "POW" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | SRB  -> "SRB" 
  | SLB  -> "SLB" 
  | SEMICOLON  -> "SEMICOLON" 
  | TT  -> "TT" 
  | FF  -> "FF" 
  | UAND  -> "UAND" 
  | UOR  -> "UOR" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | NOT  -> "NOT" 
  | EQ  -> "EQ" 
  | NEQ  -> "NEQ" 
  | GT  -> "GT" 
  | GTE  -> "GTE" 
  | LT  -> "LT" 
  | LTE  -> "LTE" 
  | SKIP  -> "SKIP" 
  | IF  -> "IF" 
  | DO  -> "DO" 
  | ENDDO  -> "ENDDO" 
  | ENDIF  -> "ENDIF" 
  | ARROW  -> "ARROW" 
  | COMPOSE  -> "COMPOSE" 
  | EOF  -> "EOF" 
  | VARNAME _ -> "VARNAME" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | ASSGN  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | SRB  -> (null : System.Object) 
  | SLB  -> (null : System.Object) 
  | SEMICOLON  -> (null : System.Object) 
  | TT  -> (null : System.Object) 
  | FF  -> (null : System.Object) 
  | UAND  -> (null : System.Object) 
  | UOR  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | NEQ  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | GTE  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | LTE  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | ENDDO  -> (null : System.Object) 
  | ENDIF  -> (null : System.Object) 
  | ARROW  -> (null : System.Object) 
  | COMPOSE  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | VARNAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 27us; 65535us; 6us; 7us; 8us; 9us; 10us; 11us; 30us; 13us; 31us; 14us; 32us; 15us; 33us; 16us; 34us; 17us; 35us; 18us; 36us; 19us; 47us; 20us; 48us; 20us; 49us; 20us; 50us; 20us; 51us; 20us; 52us; 21us; 53us; 22us; 54us; 23us; 55us; 24us; 56us; 25us; 57us; 26us; 59us; 20us; 62us; 20us; 66us; 27us; 67us; 28us; 69us; 29us; 75us; 20us; 9us; 65535us; 36us; 45us; 47us; 40us; 48us; 41us; 49us; 42us; 50us; 43us; 51us; 44us; 59us; 46us; 62us; 46us; 75us; 46us; 3us; 65535us; 0us; 2us; 73us; 71us; 76us; 72us; 3us; 65535us; 59us; 60us; 62us; 63us; 75us; 74us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 31us; 41us; 45us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 32us; 1us; 1us; 2us; 2us; 6us; 1us; 3us; 1us; 4us; 6us; 4us; 7us; 8us; 9us; 10us; 11us; 1us; 5us; 6us; 5us; 7us; 8us; 9us; 10us; 11us; 1us; 6us; 6us; 6us; 7us; 8us; 9us; 10us; 11us; 1us; 6us; 6us; 7us; 7us; 8us; 9us; 10us; 11us; 6us; 7us; 8us; 8us; 9us; 10us; 11us; 6us; 7us; 8us; 9us; 9us; 10us; 11us; 6us; 7us; 8us; 9us; 10us; 10us; 11us; 6us; 7us; 8us; 9us; 10us; 11us; 11us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 12us; 7us; 8us; 9us; 10us; 11us; 12us; 20us; 21us; 22us; 23us; 24us; 25us; 11us; 7us; 8us; 9us; 10us; 11us; 20us; 21us; 22us; 23us; 24us; 25us; 6us; 7us; 8us; 9us; 10us; 11us; 20us; 6us; 7us; 8us; 9us; 10us; 11us; 21us; 6us; 7us; 8us; 9us; 10us; 11us; 22us; 6us; 7us; 8us; 9us; 10us; 11us; 23us; 6us; 7us; 8us; 9us; 10us; 11us; 24us; 6us; 7us; 8us; 9us; 10us; 11us; 25us; 6us; 7us; 8us; 9us; 10us; 11us; 29us; 6us; 7us; 8us; 9us; 10us; 11us; 30us; 6us; 7us; 8us; 9us; 10us; 11us; 30us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 12us; 2us; 12us; 26us; 1us; 12us; 1us; 13us; 1us; 14us; 5us; 15us; 15us; 16us; 17us; 18us; 5us; 15us; 16us; 16us; 17us; 18us; 5us; 15us; 16us; 17us; 17us; 18us; 5us; 15us; 16us; 17us; 18us; 18us; 5us; 15us; 16us; 17us; 18us; 19us; 5us; 15us; 16us; 17us; 18us; 26us; 5us; 15us; 16us; 17us; 18us; 34us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 2us; 27us; 33us; 1us; 27us; 1us; 28us; 2us; 28us; 33us; 1us; 28us; 2us; 29us; 30us; 1us; 29us; 1us; 30us; 1us; 30us; 1us; 30us; 1us; 31us; 2us; 32us; 32us; 2us; 32us; 34us; 1us; 32us; 2us; 33us; 33us; 1us; 33us; 1us; 34us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 12us; 14us; 16us; 23us; 25us; 32us; 34us; 41us; 43us; 50us; 57us; 64us; 71us; 78us; 85us; 98us; 110us; 117us; 124us; 131us; 138us; 145us; 152us; 159us; 166us; 173us; 175us; 177us; 179us; 181us; 183us; 185us; 188us; 190us; 192us; 194us; 200us; 206us; 212us; 218us; 224us; 230us; 236us; 238us; 240us; 242us; 244us; 246us; 248us; 250us; 252us; 254us; 256us; 258us; 260us; 262us; 265us; 267us; 269us; 272us; 274us; 277us; 279us; 281us; 283us; 285us; 287us; 290us; 293us; 295us; 298us; 300us; |]
let _fsyacc_action_rows = 77
let _fsyacc_actionTableElements = [|4us; 32768us; 24us; 70us; 25us; 59us; 26us; 62us; 32us; 65us; 0us; 49152us; 2us; 32768us; 10us; 73us; 31us; 3us; 0us; 16385us; 1us; 16386us; 9us; 10us; 0us; 16387us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 1us; 16388us; 5us; 34us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 1us; 16389us; 5us; 34us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 6us; 32768us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 8us; 12us; 0us; 16390us; 1us; 16391us; 5us; 34us; 1us; 16392us; 5us; 34us; 1us; 16393us; 5us; 34us; 1us; 16394us; 5us; 34us; 1us; 16395us; 5us; 34us; 6us; 32768us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 7us; 37us; 12us; 32768us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 7us; 37us; 18us; 52us; 19us; 53us; 20us; 54us; 21us; 55us; 22us; 56us; 23us; 57us; 11us; 32768us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 18us; 52us; 19us; 53us; 20us; 54us; 21us; 55us; 22us; 56us; 23us; 57us; 5us; 16404us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 16405us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 16406us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 16407us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 16408us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 16409us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 16413us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 6us; 32768us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 8us; 68us; 5us; 16414us; 1us; 32us; 2us; 33us; 3us; 30us; 4us; 31us; 5us; 34us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 0us; 16396us; 0us; 16397us; 0us; 16398us; 0us; 16399us; 0us; 16400us; 0us; 16401us; 0us; 16402us; 0us; 16403us; 5us; 32768us; 7us; 58us; 13us; 47us; 14us; 48us; 15us; 49us; 16us; 50us; 5us; 32768us; 13us; 47us; 14us; 48us; 15us; 49us; 16us; 50us; 29us; 76us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 0us; 16410us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 2us; 32768us; 28us; 61us; 30us; 75us; 0us; 16411us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 2us; 32768us; 27us; 64us; 30us; 75us; 0us; 16412us; 2us; 32768us; 0us; 66us; 9us; 67us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 1us; 32768us; 0us; 69us; 5us; 32768us; 3us; 8us; 4us; 6us; 6us; 35us; 32us; 4us; 33us; 5us; 0us; 16415us; 1us; 16416us; 10us; 73us; 1us; 16418us; 10us; 73us; 4us; 32768us; 24us; 70us; 25us; 59us; 26us; 62us; 32us; 65us; 1us; 16417us; 30us; 75us; 8us; 32768us; 3us; 8us; 4us; 6us; 6us; 36us; 11us; 38us; 12us; 39us; 17us; 51us; 32us; 4us; 33us; 5us; 4us; 32768us; 24us; 70us; 25us; 59us; 26us; 62us; 32us; 65us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 9us; 10us; 12us; 13us; 19us; 21us; 27us; 29us; 35us; 42us; 43us; 45us; 47us; 49us; 51us; 53us; 60us; 73us; 85us; 91us; 97us; 103us; 109us; 115us; 121us; 127us; 134us; 140us; 146us; 152us; 158us; 164us; 170us; 176us; 185us; 186us; 187us; 188us; 189us; 190us; 191us; 192us; 193us; 199us; 205us; 214us; 223us; 232us; 241us; 250us; 256us; 262us; 268us; 274us; 280us; 286us; 287us; 296us; 299us; 300us; 309us; 312us; 313us; 316us; 322us; 328us; 330us; 336us; 337us; 339us; 341us; 346us; 348us; 357us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 2us; 2us; 4us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 1us; 3us; 3us; 3us; 3us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 6us; 1us; 3us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16396us; 16397us; 16398us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16410us; 65535us; 65535us; 16411us; 65535us; 65535us; 16412us; 65535us; 65535us; 65535us; 65535us; 65535us; 16415us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 302 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 311 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "GCLParser.fsp"
                                                _1 
                   )
# 25 "GCLParser.fsp"
                 : command));
# 322 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "GCLParser.fsp"
                                                     VAR(_1) 
                   )
# 29 "GCLParser.fsp"
                 : aexpr));
# 333 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "GCLParser.fsp"
                                                     Num(_1) 
                   )
# 30 "GCLParser.fsp"
                 : aexpr));
# 344 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "GCLParser.fsp"
                                                     UMinusExpr(_2) 
                   )
# 31 "GCLParser.fsp"
                 : aexpr));
# 355 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "GCLParser.fsp"
                                                     UPlusExpr(_2) 
                   )
# 32 "GCLParser.fsp"
                 : aexpr));
# 366 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "GCLParser.fsp"
                                                   Array(_1,_3) 
                   )
# 33 "GCLParser.fsp"
                 : aexpr));
# 378 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "GCLParser.fsp"
                                                     PlusExpr(_1,_3) 
                   )
# 34 "GCLParser.fsp"
                 : aexpr));
# 390 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "GCLParser.fsp"
                                                     MinusExpr(_1,_3) 
                   )
# 35 "GCLParser.fsp"
                 : aexpr));
# 402 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "GCLParser.fsp"
                                                     TimesExpr(_1,_3) 
                   )
# 36 "GCLParser.fsp"
                 : aexpr));
# 414 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "GCLParser.fsp"
                                                     DivExpr(_1,_3) 
                   )
# 37 "GCLParser.fsp"
                 : aexpr));
# 426 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "GCLParser.fsp"
                                                     PowExpr(_1,_3) 
                   )
# 38 "GCLParser.fsp"
                 : aexpr));
# 438 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "GCLParser.fsp"
                                                     _2 
                   )
# 39 "GCLParser.fsp"
                 : aexpr));
# 449 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "GCLParser.fsp"
                                T 
                   )
# 43 "GCLParser.fsp"
                 : bexpr));
# 459 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "GCLParser.fsp"
                                F 
                   )
# 44 "GCLParser.fsp"
                 : bexpr));
# 469 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "GCLParser.fsp"
                                      UANDExpr(_1,_3) 
                   )
# 45 "GCLParser.fsp"
                 : bexpr));
# 481 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "GCLParser.fsp"
                                     UORExpr(_1,_3) 
                   )
# 46 "GCLParser.fsp"
                 : bexpr));
# 493 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "GCLParser.fsp"
                                     ANDExpr(_1,_3) 
                   )
# 47 "GCLParser.fsp"
                 : bexpr));
# 505 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "GCLParser.fsp"
                                    ORExpr(_1,_3) 
                   )
# 48 "GCLParser.fsp"
                 : bexpr));
# 517 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "GCLParser.fsp"
                                   NOTExpr(_2) 
                   )
# 49 "GCLParser.fsp"
                 : bexpr));
# 528 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "GCLParser.fsp"
                                    EQExpr(_1,_3) 
                   )
# 50 "GCLParser.fsp"
                 : bexpr));
# 540 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "GCLParser.fsp"
                                     NEQExpr(_1,_3) 
                   )
# 51 "GCLParser.fsp"
                 : bexpr));
# 552 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "GCLParser.fsp"
                                    GTExpr(_1,_3) 
                   )
# 52 "GCLParser.fsp"
                 : bexpr));
# 564 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "GCLParser.fsp"
                                     GTEExpr(_1,_3) 
                   )
# 53 "GCLParser.fsp"
                 : bexpr));
# 576 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "GCLParser.fsp"
                                    LTExpr(_1,_3) 
                   )
# 54 "GCLParser.fsp"
                 : bexpr));
# 588 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "GCLParser.fsp"
                                     LTEqExpr(_1,_3) 
                   )
# 55 "GCLParser.fsp"
                 : bexpr));
# 600 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "GCLParser.fsp"
                                         _2 
                   )
# 56 "GCLParser.fsp"
                 : bexpr));
# 611 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : gCommand)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "GCLParser.fsp"
                                         If(_2) 
                   )
# 61 "GCLParser.fsp"
                 : command));
# 622 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : gCommand)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "GCLParser.fsp"
                                         Do(_2) 
                   )
# 62 "GCLParser.fsp"
                 : command));
# 633 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "GCLParser.fsp"
                                             Assign(_1,_3) 
                   )
# 63 "GCLParser.fsp"
                 : command));
# 645 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "GCLParser.fsp"
                                                       AssignAt(_1,_3,_6) 
                   )
# 64 "GCLParser.fsp"
                 : command));
# 658 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "GCLParser.fsp"
                                  Skip 
                   )
# 65 "GCLParser.fsp"
                 : command));
# 668 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "GCLParser.fsp"
                                           Compose(_1,_3) 
                   )
# 66 "GCLParser.fsp"
                 : command));
# 680 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : gCommand)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : gCommand)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "GCLParser.fsp"
                                           ExecuteGC(_1,_3) 
                   )
# 71 "GCLParser.fsp"
                 : gCommand));
# 692 "GCLParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "GCLParser.fsp"
                                       ExecuteIf(_1,_3) 
                   )
# 72 "GCLParser.fsp"
                 : gCommand));
|]
# 705 "GCLParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 37;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : command =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
