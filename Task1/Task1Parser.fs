// Implementation file for parser generated by fsyacc
module Task1Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "Task1Parser.fsp"

open Task1TypesAST

# 10 "Task1Parser.fs"
// This type is the type of tokens accepted by the parser
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
// This type is used to give symbolic names to token indexes, useful for error messages
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
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_guardedCommand
    | NONTERM_command
    | NONTERM_boolean
    | NONTERM_predicate

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | IMMUTABLE  -> 0 
  | PRED  -> 1 
  | IF  -> 2 
  | FI  -> 3 
  | DO  -> 4 
  | OD  -> 5 
  | LSQPAR  -> 6 
  | RSQPAR  -> 7 
  | TRUE  -> 8 
  | FALSE  -> 9 
  | NOTEQUAL  -> 10 
  | NOT  -> 11 
  | EQUAL  -> 12 
  | GEQ  -> 13 
  | GT  -> 14 
  | LEQ  -> 15 
  | LT  -> 16 
  | EOF  -> 17 
  | BFUNCTION  -> 18 
  | TWOGUARD  -> 19 
  | DAND  -> 20 
  | AND  -> 21 
  | DOR  -> 22 
  | OR  -> 23 
  | LPAR  -> 24 
  | RPAR  -> 25 
  | ASSIGN  -> 26 
  | SKIP  -> 27 
  | COMMANDLIST  -> 28 
  | TIMES  -> 29 
  | DIV  -> 30 
  | PLUS  -> 31 
  | MINUS  -> 32 
  | UMINUS  -> 33 
  | POW  -> 34 
  | VAR _ -> 35 
  | NUM _ -> 36 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_IMMUTABLE 
  | 1 -> TOKEN_PRED 
  | 2 -> TOKEN_IF 
  | 3 -> TOKEN_FI 
  | 4 -> TOKEN_DO 
  | 5 -> TOKEN_OD 
  | 6 -> TOKEN_LSQPAR 
  | 7 -> TOKEN_RSQPAR 
  | 8 -> TOKEN_TRUE 
  | 9 -> TOKEN_FALSE 
  | 10 -> TOKEN_NOTEQUAL 
  | 11 -> TOKEN_NOT 
  | 12 -> TOKEN_EQUAL 
  | 13 -> TOKEN_GEQ 
  | 14 -> TOKEN_GT 
  | 15 -> TOKEN_LEQ 
  | 16 -> TOKEN_LT 
  | 17 -> TOKEN_EOF 
  | 18 -> TOKEN_BFUNCTION 
  | 19 -> TOKEN_TWOGUARD 
  | 20 -> TOKEN_DAND 
  | 21 -> TOKEN_AND 
  | 22 -> TOKEN_DOR 
  | 23 -> TOKEN_OR 
  | 24 -> TOKEN_LPAR 
  | 25 -> TOKEN_RPAR 
  | 26 -> TOKEN_ASSIGN 
  | 27 -> TOKEN_SKIP 
  | 28 -> TOKEN_COMMANDLIST 
  | 29 -> TOKEN_TIMES 
  | 30 -> TOKEN_DIV 
  | 31 -> TOKEN_PLUS 
  | 32 -> TOKEN_MINUS 
  | 33 -> TOKEN_UMINUS 
  | 34 -> TOKEN_POW 
  | 35 -> TOKEN_VAR 
  | 36 -> TOKEN_NUM 
  | 39 -> TOKEN_end_of_input
  | 37 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_expression 
    | 3 -> NONTERM_expression 
    | 4 -> NONTERM_expression 
    | 5 -> NONTERM_expression 
    | 6 -> NONTERM_expression 
    | 7 -> NONTERM_expression 
    | 8 -> NONTERM_expression 
    | 9 -> NONTERM_expression 
    | 10 -> NONTERM_expression 
    | 11 -> NONTERM_expression 
    | 12 -> NONTERM_expression 
    | 13 -> NONTERM_expression 
    | 14 -> NONTERM_guardedCommand 
    | 15 -> NONTERM_guardedCommand 
    | 16 -> NONTERM_command 
    | 17 -> NONTERM_command 
    | 18 -> NONTERM_command 
    | 19 -> NONTERM_command 
    | 20 -> NONTERM_command 
    | 21 -> NONTERM_command 
    | 22 -> NONTERM_boolean 
    | 23 -> NONTERM_boolean 
    | 24 -> NONTERM_boolean 
    | 25 -> NONTERM_boolean 
    | 26 -> NONTERM_boolean 
    | 27 -> NONTERM_boolean 
    | 28 -> NONTERM_boolean 
    | 29 -> NONTERM_boolean 
    | 30 -> NONTERM_boolean 
    | 31 -> NONTERM_boolean 
    | 32 -> NONTERM_boolean 
    | 33 -> NONTERM_boolean 
    | 34 -> NONTERM_boolean 
    | 35 -> NONTERM_boolean 
    | 36 -> NONTERM_predicate 
    | 37 -> NONTERM_predicate 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 39 
let _fsyacc_tagOfErrorTerminal = 37

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | IMMUTABLE  -> "IMMUTABLE" 
  | PRED  -> "PRED" 
  | IF  -> "IF" 
  | FI  -> "FI" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | LSQPAR  -> "LSQPAR" 
  | RSQPAR  -> "RSQPAR" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | NOTEQUAL  -> "NOTEQUAL" 
  | NOT  -> "NOT" 
  | EQUAL  -> "EQUAL" 
  | GEQ  -> "GEQ" 
  | GT  -> "GT" 
  | LEQ  -> "LEQ" 
  | LT  -> "LT" 
  | EOF  -> "EOF" 
  | BFUNCTION  -> "BFUNCTION" 
  | TWOGUARD  -> "TWOGUARD" 
  | DAND  -> "DAND" 
  | AND  -> "AND" 
  | DOR  -> "DOR" 
  | OR  -> "OR" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | ASSIGN  -> "ASSIGN" 
  | SKIP  -> "SKIP" 
  | COMMANDLIST  -> "COMMANDLIST" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | UMINUS  -> "UMINUS" 
  | POW  -> "POW" 
  | VAR _ -> "VAR" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | IMMUTABLE  -> (null : System.Object) 
  | PRED  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | FI  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | LSQPAR  -> (null : System.Object) 
  | RSQPAR  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | NOTEQUAL  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | GEQ  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LEQ  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | BFUNCTION  -> (null : System.Object) 
  | TWOGUARD  -> (null : System.Object) 
  | DAND  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | DOR  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | COMMANDLIST  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | UMINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 27us; 65535us; 24us; 4us; 25us; 5us; 26us; 6us; 27us; 7us; 28us; 8us; 29us; 9us; 30us; 10us; 33us; 11us; 37us; 12us; 38us; 13us; 46us; 17us; 48us; 14us; 49us; 15us; 51us; 16us; 55us; 17us; 57us; 17us; 67us; 17us; 68us; 17us; 69us; 17us; 70us; 17us; 71us; 17us; 72us; 18us; 73us; 19us; 74us; 20us; 75us; 21us; 76us; 22us; 77us; 23us; 3us; 65535us; 46us; 43us; 55us; 44us; 57us; 45us; 3us; 65535us; 0us; 2us; 41us; 42us; 54us; 53us; 9us; 65535us; 38us; 66us; 46us; 40us; 55us; 40us; 57us; 40us; 67us; 61us; 68us; 62us; 69us; 63us; 70us; 64us; 71us; 65us; 0us; 65535us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 31us; 35us; 39us; 49us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 19us; 1us; 1us; 6us; 2us; 2us; 3us; 4us; 6us; 7us; 6us; 2us; 3us; 3us; 4us; 6us; 7us; 6us; 2us; 3us; 4us; 4us; 6us; 7us; 6us; 2us; 3us; 4us; 5us; 6us; 7us; 6us; 2us; 3us; 4us; 6us; 6us; 7us; 6us; 2us; 3us; 4us; 6us; 7us; 7us; 6us; 2us; 3us; 4us; 6us; 7us; 8us; 6us; 2us; 3us; 4us; 6us; 7us; 10us; 6us; 2us; 3us; 4us; 6us; 7us; 13us; 12us; 2us; 3us; 4us; 6us; 7us; 13us; 29us; 30us; 31us; 32us; 33us; 34us; 6us; 2us; 3us; 4us; 6us; 7us; 16us; 6us; 2us; 3us; 4us; 6us; 7us; 17us; 6us; 2us; 3us; 4us; 6us; 7us; 17us; 11us; 2us; 3us; 4us; 6us; 7us; 29us; 30us; 31us; 32us; 33us; 34us; 6us; 2us; 3us; 4us; 6us; 7us; 29us; 6us; 2us; 3us; 4us; 6us; 7us; 30us; 6us; 2us; 3us; 4us; 6us; 7us; 31us; 6us; 2us; 3us; 4us; 6us; 7us; 32us; 6us; 2us; 3us; 4us; 6us; 7us; 33us; 6us; 2us; 3us; 4us; 6us; 7us; 34us; 1us; 2us; 1us; 3us; 1us; 4us; 1us; 5us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 9us; 2us; 10us; 11us; 1us; 10us; 1us; 10us; 1us; 12us; 1us; 12us; 1us; 13us; 2us; 13us; 35us; 1us; 13us; 5us; 14us; 24us; 25us; 26us; 27us; 1us; 14us; 2us; 14us; 19us; 2us; 15us; 15us; 2us; 15us; 20us; 2us; 15us; 21us; 1us; 15us; 2us; 16us; 17us; 1us; 16us; 1us; 17us; 1us; 17us; 1us; 17us; 1us; 18us; 2us; 19us; 19us; 1us; 19us; 1us; 20us; 1us; 20us; 1us; 21us; 1us; 21us; 1us; 22us; 1us; 23us; 5us; 24us; 24us; 25us; 26us; 27us; 5us; 24us; 25us; 25us; 26us; 27us; 5us; 24us; 25us; 26us; 26us; 27us; 5us; 24us; 25us; 26us; 27us; 27us; 5us; 24us; 25us; 26us; 27us; 28us; 5us; 24us; 25us; 26us; 27us; 35us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; 1us; 32us; 1us; 33us; 1us; 34us; 1us; 35us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 16us; 23us; 30us; 37us; 44us; 51us; 58us; 65us; 72us; 85us; 92us; 99us; 106us; 118us; 125us; 132us; 139us; 146us; 153us; 160us; 162us; 164us; 166us; 168us; 170us; 172us; 174us; 176us; 179us; 181us; 183us; 185us; 187us; 189us; 192us; 194us; 200us; 202us; 205us; 208us; 211us; 214us; 216us; 219us; 221us; 223us; 225us; 227us; 229us; 232us; 234us; 236us; 238us; 240us; 242us; 244us; 246us; 252us; 258us; 264us; 270us; 276us; 282us; 284us; 286us; 288us; 290us; 292us; 294us; 296us; 298us; 300us; 302us; 304us; |]
let _fsyacc_action_rows = 79
let _fsyacc_actionTableElements = [|4us; 32768us; 2us; 55us; 4us; 57us; 27us; 52us; 35us; 47us; 0us; 49152us; 2us; 32768us; 17us; 3us; 28us; 54us; 0us; 16385us; 1us; 16386us; 34us; 29us; 1us; 16387us; 34us; 29us; 3us; 16388us; 29us; 24us; 30us; 25us; 34us; 29us; 3us; 16389us; 29us; 24us; 30us; 25us; 34us; 29us; 3us; 16390us; 29us; 24us; 30us; 25us; 34us; 29us; 1us; 16391us; 34us; 29us; 3us; 16392us; 29us; 24us; 30us; 25us; 34us; 29us; 6us; 32768us; 7us; 34us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 6us; 32768us; 25us; 39us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 12us; 32768us; 10us; 73us; 12us; 72us; 13us; 74us; 14us; 75us; 15us; 76us; 16us; 77us; 25us; 39us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16400us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 6us; 32768us; 7us; 50us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16401us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 11us; 32768us; 10us; 73us; 12us; 72us; 13us; 74us; 14us; 75us; 15us; 76us; 16us; 77us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16413us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16414us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16415us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16416us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16417us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 5us; 16418us; 29us; 24us; 30us; 25us; 31us; 26us; 32us; 28us; 34us; 29us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16393us; 1us; 16395us; 6us; 33us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16394us; 1us; 32768us; 35us; 36us; 0us; 16396us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16397us; 5us; 32768us; 18us; 41us; 20us; 68us; 21us; 67us; 22us; 70us; 23us; 69us; 4us; 32768us; 2us; 55us; 4us; 57us; 27us; 52us; 35us; 47us; 1us; 16398us; 28us; 54us; 1us; 16399us; 19us; 46us; 2us; 32768us; 3us; 56us; 19us; 46us; 2us; 32768us; 5us; 58us; 19us; 46us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 2us; 32768us; 6us; 49us; 26us; 48us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 1us; 32768us; 26us; 51us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16402us; 1us; 16403us; 28us; 54us; 4us; 32768us; 2us; 55us; 4us; 57us; 27us; 52us; 35us; 47us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16404us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16405us; 0us; 16406us; 0us; 16407us; 0us; 16408us; 0us; 16409us; 2us; 16410us; 20us; 68us; 21us; 67us; 2us; 16411us; 20us; 68us; 21us; 67us; 0us; 16412us; 5us; 32768us; 20us; 68us; 21us; 67us; 22us; 70us; 23us; 69us; 25us; 78us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 9us; 32768us; 0us; 35us; 8us; 59us; 9us; 60us; 11us; 71us; 24us; 38us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 6us; 32768us; 0us; 35us; 24us; 37us; 31us; 30us; 32us; 27us; 35us; 32us; 36us; 31us; 0us; 16419us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 9us; 10us; 12us; 14us; 18us; 22us; 26us; 28us; 32us; 39us; 46us; 59us; 65us; 72us; 78us; 90us; 96us; 102us; 108us; 114us; 120us; 126us; 133us; 140us; 147us; 154us; 161us; 168us; 175us; 176us; 178us; 185us; 186us; 188us; 189us; 196us; 206us; 207us; 213us; 218us; 220us; 222us; 225us; 228us; 238us; 241us; 248us; 255us; 257us; 264us; 265us; 267us; 272us; 282us; 283us; 293us; 294us; 295us; 296us; 297us; 298us; 301us; 304us; 305us; 311us; 321us; 331us; 341us; 351us; 361us; 368us; 375us; 382us; 389us; 396us; 403us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 3us; 3us; 2us; 3us; 3us; 2us; 1us; 4us; 1us; 2us; 3us; 3us; 3us; 3us; 6us; 1us; 3us; 3us; 3us; 1us; 1us; 3us; 3us; 3us; 3us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 2us; 2us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 6us; 6us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16393us; 65535us; 65535us; 16394us; 65535us; 16396us; 65535us; 65535us; 16397us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16402us; 65535us; 65535us; 65535us; 16404us; 65535us; 16405us; 16406us; 16407us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16419us; |]
let _fsyacc_reductions ()  =    [| 
# 324 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 333 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Task1Parser.fsp"
                                                      _1 
                   )
# 49 "Task1Parser.fsp"
                 : command));
# 344 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Task1Parser.fsp"
                                                         TimesExpr(_1,_3) 
                   )
# 59 "Task1Parser.fsp"
                 : aexpr));
# 356 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Task1Parser.fsp"
                                                         DivExpr(_1,_3) 
                   )
# 60 "Task1Parser.fsp"
                 : aexpr));
# 368 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Task1Parser.fsp"
                                                         PlusExpr(_1,_3) 
                   )
# 61 "Task1Parser.fsp"
                 : aexpr));
# 380 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Task1Parser.fsp"
                                                UMinusExpr(_2)
                   )
# 62 "Task1Parser.fsp"
                 : aexpr));
# 391 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Task1Parser.fsp"
                                                         MinusExpr(_1,_3) 
                   )
# 63 "Task1Parser.fsp"
                 : aexpr));
# 403 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Task1Parser.fsp"
                                                         PowExpr(_1,_3) 
                   )
# 64 "Task1Parser.fsp"
                 : aexpr));
# 415 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Task1Parser.fsp"
                                                         UPlusExpr(_2) 
                   )
# 65 "Task1Parser.fsp"
                 : aexpr));
# 426 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "Task1Parser.fsp"
                                                         Num(_1) 
                   )
# 66 "Task1Parser.fsp"
                 : aexpr));
# 437 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Task1Parser.fsp"
                                                         Arr(_1, _3) 
                   )
# 67 "Task1Parser.fsp"
                 : aexpr));
# 449 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "Task1Parser.fsp"
                                       X(_1)
                   )
# 68 "Task1Parser.fsp"
                 : aexpr));
# 460 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Task1Parser.fsp"
                                              X'(_2)
                   )
# 69 "Task1Parser.fsp"
                 : aexpr));
# 471 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Task1Parser.fsp"
                                                   APar(_2)
                   )
# 70 "Task1Parser.fsp"
                 : aexpr));
# 482 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "Task1Parser.fsp"
                                                       Bfunc(_1, _3)
                   )
# 74 "Task1Parser.fsp"
                 : guard));
# 494 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : guard)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : guard)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Task1Parser.fsp"
                                                                  Twoguard(_1, _3)
                   )
# 75 "Task1Parser.fsp"
                 : guard));
# 506 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Task1Parser.fsp"
                                                      Assign(_1,_3)
                   )
# 79 "Task1Parser.fsp"
                 : command));
# 518 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Task1Parser.fsp"
                                                                          AssignArr(_1, _3, _6)
                   )
# 80 "Task1Parser.fsp"
                 : command));
# 531 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "Task1Parser.fsp"
                                         Skip
                   )
# 81 "Task1Parser.fsp"
                 : command));
# 541 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "Task1Parser.fsp"
                                                           CommandList(_1,_3)
                   )
# 82 "Task1Parser.fsp"
                 : command));
# 553 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : guard)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "Task1Parser.fsp"
                                                     Ifstate(_2)
                   )
# 83 "Task1Parser.fsp"
                 : command));
# 564 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : guard)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "Task1Parser.fsp"
                                                     Dostate(_2)
                   )
# 84 "Task1Parser.fsp"
                 : command));
# 575 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "Task1Parser.fsp"
                                     True
                   )
# 88 "Task1Parser.fsp"
                 : b));
# 585 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 89 "Task1Parser.fsp"
                                      False
                   )
# 89 "Task1Parser.fsp"
                 : b));
# 595 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "Task1Parser.fsp"
                                                 And(_1, _3)
                   )
# 90 "Task1Parser.fsp"
                 : b));
# 607 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "Task1Parser.fsp"
                                                 DAnd(_1, _3)
                   )
# 91 "Task1Parser.fsp"
                 : b));
# 619 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 92 "Task1Parser.fsp"
                                                Or(_1, _3)
                   )
# 92 "Task1Parser.fsp"
                 : b));
# 631 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 93 "Task1Parser.fsp"
                                                 DOr(_1, _3)
                   )
# 93 "Task1Parser.fsp"
                 : b));
# 643 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "Task1Parser.fsp"
                                           Not(_2)
                   )
# 94 "Task1Parser.fsp"
                 : b));
# 654 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "Task1Parser.fsp"
                                                       Equal(_1, _3)
                   )
# 95 "Task1Parser.fsp"
                 : b));
# 666 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 96 "Task1Parser.fsp"
                                                          NotEqual(_1, _3)
                   )
# 96 "Task1Parser.fsp"
                 : b));
# 678 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 97 "Task1Parser.fsp"
                                                     Geq(_1, _3)
                   )
# 97 "Task1Parser.fsp"
                 : b));
# 690 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 98 "Task1Parser.fsp"
                                                    Gt(_1, _3)
                   )
# 98 "Task1Parser.fsp"
                 : b));
# 702 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 99 "Task1Parser.fsp"
                                                     Leq(_1,_3)
                   )
# 99 "Task1Parser.fsp"
                 : b));
# 714 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 100 "Task1Parser.fsp"
                                                    Lt(_1,_3)
                   )
# 100 "Task1Parser.fsp"
                 : b));
# 726 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 101 "Task1Parser.fsp"
                                               ParB(_2)
                   )
# 101 "Task1Parser.fsp"
                 : b));
# 737 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : b)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 105 "Task1Parser.fsp"
                                          PBool(_2)
                   )
# 105 "Task1Parser.fsp"
                 : 'predicate));
# 748 "Task1Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : aexpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 106 "Task1Parser.fsp"
                                          Pexpr(_2)
                   )
# 106 "Task1Parser.fsp"
                 : 'predicate));
|]
# 760 "Task1Parser.fs"
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
    numTerminals = 40;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : command =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
