// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module Task1TypesAST
type aexpr =
  | Num of (int)
  | X of (string)
  | X' of (string)
  | Arr of (string * aexpr)
  | TimesExpr of (aexpr * aexpr)
  | DivExpr of (aexpr * aexpr)
  | PlusExpr of (aexpr * aexpr)
  | MinusExpr of (aexpr * aexpr)
  | PowExpr of (aexpr * aexpr)
  | UPlusExpr of (aexpr)
  | UMinusExpr of (aexpr)
  | APar of (aexpr)


type command = 
    | Assign of (string * aexpr)
    | AssignArr of (string * aexpr *  aexpr)
    | Skip
    | CommandList of (command * command)
    | Ifstate of (guard)
    | Dostate of (guard)
and b =
    | True
    | False
    | And of (b * b)
    | DAnd of (b * b)
    | Or of (b * b)
    | DOr of (b * b)
    | Not of (b)
    | Equal of (aexpr * aexpr)
    | NotEqual of (aexpr * aexpr)
    | Geq of (aexpr * aexpr)
    | Gt of (aexpr * aexpr)
    | Lt of (aexpr * aexpr)
    | Leq of (aexpr * aexpr)
    | ParB of (b)
and guard = 
    | Bfunc of (b * command)
    | Twoguard of (guard * guard)

type p = 
    | PBool of (b)
    | Pexpr of (aexpr)

//type p = 
//    | True
//    | False
//    | And of (p * p)
//    | Or of (p * p)
//    | Not of (p)
//    | Implies of (p * p)
//    | Equal of (pexpr * pexpr)
//    | Geq of (pexpr * pexpr)
//    | Gt of (pexpr * pexpr)
//    | Lt of (pexpr * pexpr)
//    | Leq of (pexpr * pexpr)
//    | ParB of (b)
//and pexpr = 
//    | X of aexpr
//    | X' of aexpr
    //| F of (pexpr list -> int)