// Signature file for parser generated by fsyacc
module GCLParser
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
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_a
    | NONTERM_b
    | NONTERM_C
    | NONTERM_GC
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (command) 
