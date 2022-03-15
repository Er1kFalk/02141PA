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

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
//let rec evalExp e =
//  match e with
//    | Num(x) -> x
//    | TimesExpr(x,y) -> evalExp(x) * evalExp (y)
//    | DivExpr(x,y) -> evalExp(x) / evalExp (y)
//    | PlusExpr(x,y) -> evalExp(x) + evalExp (y)
//    | MinusExpr(x,y) -> evalExp(x) - evalExp (y)
//    | PowExpr(x,y) -> pow (evalExp(x)) (evalExp (y))
//    | UPlusExpr(x) -> evalExp(x)
//    | UMinusExpr(x) -> - evalExp(x)
//and pow x y = 
//    match y with
//    | 0 -> 1
//    | _ -> x*(pow x (y-1))

// get max of list
let rec max l =
   match l with
   | [] -> 0
   | (_, y')::xs -> if y' > (max xs) then y' else max xs

// We
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
    | X(s) -> s+""
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

let prettyprinterPretext (num:int) (num2:int) (label:string) =
    if (num=0) then "q▷-> q" + string(num2) + "[label= " + label + "];"
               else "q" + (string(num)) + "-> q" + string(num2) + "[label= " + label + "];"

let rec combinedguard x=
    match x with
    | [] -> ""
    | [x'] -> x'
    | x'::xs -> x' + " and " + (combinedguard xs) 

let rec pgPrintGuard gc beginnode nextnode l boollist= 
    match gc with
    | Bfunc (b, c) -> 
        let newlist = (pgPrinter c beginnode (nextnode+1) (l))
        let newlist2 = (prettyprinterPretext beginnode (nextnode+1) (prettyprintBool b), beginnode)::newlist
        let not = prettyprintBool (Not b)
        (boollist@[not],newlist2)
    | Twoguard (g1, g2) -> 
                           let (bool,firstguard) =(pgPrintGuard g1 beginnode nextnode l boollist) 
                           
                           let (bool2,secondguard)=pgPrintGuard g2 (beginnode) (max firstguard) [] bool
                           let returnback1 = (prettyprinterPretext (max(firstguard)) (max(secondguard)) "skip",0)
                           (bool2, firstguard@(returnback1::secondguard))
and pgPrinter x (beginnode:int) (nextnode:int) list=
    match x with
    | Assign(x',y') -> ((prettyprinterPretext nextnode (nextnode+1)) (x'+":="+ prettyprintExp y'),nextnode+1)::list
    | AssignArr(x',y,z')-> ((prettyprinterPretext nextnode (nextnode+1)) (x' + prettyprintExp y + ":=" + prettyprintExp z'),nextnode)::list
    | CommandList(x',y')   -> let lx = pgPrinter x' beginnode nextnode list
                              let ly = (pgPrinter y' (max lx) (max lx) [])
                              lx @ ly
    | Ifstate(x') -> let (bool,list) = pgPrintGuard x' beginnode (nextnode) list []
                     let boollist= (prettyprinterPretext beginnode (max list+1) (combinedguard(bool)),max(list))::list 
                     boollist               
    | Dostate(x') -> let (bool,list) = pgPrintGuard x' beginnode (nextnode) list []
                     let boollist= (prettyprinterPretext beginnode (max list+1) (combinedguard(bool)),max(list)+1)::list 
                     let skip = (prettyprinterPretext (max list) (beginnode) "skip",(max(list)+1))
                     boollist@[skip]   
    | Skip -> ((prettyprinterPretext nextnode (nextnode+1)) ("skip"),nextnode+1)::list



// We implement here the function that interacts with the user
//let rec compute n =
//    if n = 0 then
//        printfn "Bye bye"
//    else
//        printf "Enter an arithmetic expression: "
//        try
//        // We parse the input string
//        let e = parse (Console.ReadLine())
//        // and print the result of evaluating it
//        printfn "Result: %d" (eval(e))
//        compute n
//        with err -> compute (n-1)

// Start interacting with the user

let rec readAll s = 
    match s with
    | "" -> ""
    | x -> x + readAll (Console.ReadLine())

let rec print  x =
    match x with
    | [] -> Console.WriteLine("")
    | (x':String,y')::xs -> Console.WriteLine(x')
                            print xs

//let parseAll =
//    Console.Write("Enter your favorite GCL expression: ")
//    let string = parse (readAll (Console.ReadLine()))
//    Console.WriteLine string
//    Console.WriteLine (prettyprintCommand string)




let run =
    Console.WriteLine("Enter your favorite GCL expression: ")
    let string = parse (readAll (Console.ReadLine()) + "\n") // parse input
    Console.WriteLine (string) // print AST
    Console.WriteLine (prettyprintCommand string) // print result parsed back
    Console.WriteLine "" 
    // print program graph
    let result =pgPrinter string 1 1 [] 
    let endpoint = max(result)  //last node used
    print ((("q▷-> q1[label= begin ];",0  )::result)@ [("q" + (sprintf "%i" endpoint) + "-> q<[label= end ];",0  )] ) // generates beginning node and endnode
   
run;;

// Console.WriteLine ("q>"+ (string(endpoint)) + " -> q< [label= skip ];")



