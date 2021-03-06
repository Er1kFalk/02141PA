// Signature file for parser generated by fsyacc
module Task1Parser
type token = 
  | IMMUTABLE
  | PRED
  | IF
  | FI
  | DO
  | OD
  | LSQPAR
  | RSQPAR
  | TRUE
  | FALSE
  | NOTEQUAL
  | NOT
  | EQUAL
  | GEQ
  | GT
  | LEQ
  | LT
  | EOF
  | BFUNCTION
  | TWOGUARD
  | DAND
  | AND
  | DOR
  | OR
  | LPAR
  | RPAR
  | ASSIGN
  | SKIP
  | COMMANDLIST
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | UMINUS
  | POW
  | VAR of (string)
  | NUM of (int)
type tokenId = 
    | TOKEN_IMMUTABLE
    | TOKEN_PRED
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_LSQPAR
    | TOKEN_RSQPAR
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NOTEQUAL
    | TOKEN_NOT
    | TOKEN_EQUAL
    | TOKEN_GEQ
    | TOKEN_GT
    | TOKEN_LEQ
    | TOKEN_LT
    | TOKEN_EOF
    | TOKEN_BFUNCTION
    | TOKEN_TWOGUARD
    | TOKEN_DAND
    | TOKEN_AND
    | TOKEN_DOR
    | TOKEN_OR
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_COMMANDLIST
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_UMINUS
    | TOKEN_POW
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_guardedCommand
    | NONTERM_command
    | NONTERM_boolean
    | NONTERM_predicate
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (command) 
