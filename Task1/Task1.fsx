// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "C:/Users/Erik/.nuget/packages/fslexyacc/10.0.0/build/fsyacc/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "Task1TypesAST.fs"
open Task1TypesAST
#load "Task1Parser.fs"
open Task1Parser
#load "Task1Lexer.fs"
open Task1Lexer

// get input 
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Task1Parser.start Task1Lexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


let rec prettyprintExp x  = 
    match x with
    | Num(n) -> string n
    | X(s) -> s
    | Arr (s, exp) -> s + "[" + (prettyprintExp exp) + "]"
    | TimesExpr (a, b) -> (prettyprintExp a) + "*" + (prettyprintExp b)
    | DivExpr (a, b) ->  (prettyprintExp a) + "/" + (prettyprintExp b)
    | PlusExpr (a, b) -> (prettyprintExp a) + "+" + (prettyprintExp b)
    | MinusExpr (a, b) -> (prettyprintExp a) + "-" + (prettyprintExp b)
    | PowExpr (a, b) -> (prettyprintExp a) + "^" + (prettyprintExp b)
    | UMinusExpr (a) -> "-" + (prettyprintExp a)
    | APar(a) -> "(" + (prettyprintExp a) + ")"
    | UPlusExpr a -> (prettyprintExp a)


let rec prettyprintBool x = 
    match x with
    | True -> "true"
    | False -> "false"
    | And (a, b) -> (prettyprintBool a) + "&" + (prettyprintBool b)
    | DAnd (a, b) -> (prettyprintBool a) + "&&" + (prettyprintBool b)
    | Or (a, b) -> (prettyprintBool a) + "|" + (prettyprintBool b)
    | DOr (a, b) -> (prettyprintBool a) + "||" + (prettyprintBool b)
    | Not b -> "!" + (prettyprintBool b)
    | Equal (a, b) -> (prettyprintExp a) + "=" + (prettyprintExp b)
    | NotEqual (a, b) -> (prettyprintExp a) + "!=" + (prettyprintExp b)
    | Geq (a, b) -> (prettyprintExp a) + ">=" + (prettyprintExp b)
    | Gt (a, b) -> (prettyprintExp a) + ">" + (prettyprintExp b)
    | Lt (a, b) -> (prettyprintExp a) + "<" + (prettyprintExp b)
    | Leq (a, b) -> (prettyprintExp a) + "<=" + (prettyprintExp b)
    | ParB b -> "(" + (prettyprintBool b) + ")"

let rec prettyprintGuard g = 
    match g with
    | Bfunc (b, c) -> (prettyprintBool b) + " -> " + (prettyprintCommand c)
    | Twoguard (g1, g2) -> (prettyprintGuard g1) + " [] " + (prettyprintGuard g2)
and prettyprintCommand c = 
    match c with
    | Assign(s, a) -> s + ":="+ (prettyprintExp a)
    | AssignArr(s, a1, a2) -> s + "[" + (prettyprintExp a1) + "]:=" + (prettyprintExp a2)
    | Skip -> "Skip"
    | CommandList (c1, c2) -> (prettyprintCommand c1) + ";\n" + (prettyprintCommand c2)
    | Ifstate g -> "if " + prettyprintGuard g + " fi"
    | Dostate g -> "do " + prettyprintGuard g + " od"

let rec readAll s = 
    match s with
    | "" -> ""
    | x -> x + readAll (Console.ReadLine())

let rec print  x =
    match x with
    | [] -> Console.WriteLine("")
    | (x':String,y')::xs -> Console.WriteLine(x')
                            print xs

let parseAll =
    Console.Write("Enter your favorite GCL expression: ")
    let string = parse (readAll (Console.ReadLine()))
    Console.WriteLine string
    Console.WriteLine (prettyprintCommand string)


parseAll;;