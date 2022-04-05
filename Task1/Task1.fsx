// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "C:/Users/Harald/.nuget/packages/fslexyacc/10.0.0/build/fsyacc/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "Task1TypesAST.fs"
open Task1TypesAST
#load "Task1Parser.fs"
open Task1Parser
#load "Task1Lexer.fs"
open Task1Lexer


type EvalList = List<string*(List<(String*int)>*List<((String*int)*int)>)>
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
let rec max2 l =
   match l with
   | [] -> 0
   | (_, y',_, _)::xs -> if y' > (max2 xs) then y' else max2 xs



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
    | X'(s) -> s+""
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
    | Not b -> let text ="!(" + (prettyprintBool b) 
               text + ")"
    | Equal (a, b) -> (prettyprintExp a) + "=" + (prettyprintExp b)
    | NotEqual (a, b) -> (prettyprintExp a) + "!=" + (prettyprintExp b)
    | Geq (a, b) -> (prettyprintExp a) + ">=" + (prettyprintExp b)
    | Gt (a, b) -> (prettyprintExp a) + ">" + (prettyprintExp b)
    | Lt (a, b) -> (prettyprintExp a) + "<" + (prettyprintExp b)
    | Leq (a, b) -> (prettyprintExp a) + "<=" + (prettyprintExp b)
    | ParB b -> "(" + (prettyprintBool b) + ")"

let prettyPrintPredicate input = 
    match input with
    | PBool b -> "{ " + prettyprintBool b + " }"

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
    | PredCommand (p1, c, p2) ->
        prettyPrintPredicate p1 + "\n" + prettyprintCommand c + "\n" + prettyPrintPredicate p2
let prettyprinterPretext (num:int) (num2:int) max (label:string) =
    if (num=0)  then "q▷-> q" + string(num2) + "[label= \"" + label + "\"];"
                else if num2=max then
                     "q" + (string(num)) + "-> q◀ [label=  \"" + label + " \"];"
                else "q" + (string(num)) + "-> q" + string(num2) + "[label=  \"" + label + "\"];"

let rec combinedguard x=
    match x with
    | [] -> ""
    | [x'] -> x'
    | x'::xs -> x' + " and " + (combinedguard xs) 

(* let rec pgPrintGuard gc beginnode nextnode l boollist= 
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
    | Skip -> ((prettyprinterPretext nextnode (nextnode+1)) ("skip"),nextnode+1)::list *)
and pgc x (beginnode:int) (nextnode:int) list stepcount maxcount covered =
    if stepcount<maxcount then
        match x with
        | Assign(x',y') -> (nextnode, (nextnode+1), (x'+":="+ prettyprintExp y'), true)::list
        | AssignArr(x',y,z')-> (nextnode, (nextnode+1), (x' + prettyprintExp y + ":=" + prettyprintExp z'), true)::list
        | CommandList(x',y')   -> let lx = pgc x' beginnode nextnode list (stepcount+1) maxcount false
                                  let ly = (pgc y' (max2 lx) (max2 lx) [] (stepcount+1) maxcount false)
                                  lx @ ly
        | Ifstate(x') -> let (bool,list) = pgg x' nextnode (nextnode) list [] (stepcount+1) maxcount
                         let boollist= ( (beginnode, ((max2 list)+1), (combinedguard(bool)),true)::list )
                         boollist               
        | Dostate(x') -> let (bool,list) = pgg x' nextnode (nextnode) list [] (stepcount+1) maxcount
                         let skip = list @ [((max2 list), (beginnode), "skip", covered)]
                         let boollist= (skip @ [(beginnode, (max2 list+1), (combinedguard(bool)), covered )] )
                         boollist  
        | Skip -> (nextnode, (nextnode+1), ("skip"), true)::list
        | PredCommand (_, c, _) ->
            pgc c beginnode nextnode list stepcount maxcount true
    else 
        failwith "to many steps taken"
and pgg gc beginnode nextnode l boollist stepcount maxcount=
    if stepcount<maxcount then 
        match gc with
            | Bfunc (b, c) -> 
                let newlist = (pgc c beginnode (nextnode+1) (l) (stepcount+1) maxcount false)
                let newlist2 = (beginnode, (nextnode+1), (prettyprintBool b), true)::newlist
                let not = prettyprintBool (Not b)
                (not::boollist,newlist2)
            | Twoguard (g1, g2) -> 
                let (bool,firstguard) =(pgg g1 beginnode nextnode l boollist (stepcount+1) maxcount) 
                let (bool2,secondguard)=pgg g2 (beginnode) (max2 firstguard) [] bool (stepcount+1) maxcount
                let returnback1 = ((max2(firstguard)), (beginnode), "skip", true)
                (bool2, firstguard@(returnback1::secondguard))
    else 
        failwith "to many steps taken"                           


(* We implement here the function that interacts with the user
let rec compute n =
   if n = 0 then
       printfn "Bye bye"
   else
       printf "Enter an arithmetic expression: "
       try
       // We parse the input string
       let e = parse (Console.ReadLine())
       // and print the result of evaluating it
       printfn "Result: %d" (eval(e))
       compute n
       with err -> compute (n-1)

Start interacting with the user *)

let rec readAll s = 
    match s with
    | "" -> ""
    | x -> x + readAll (Console.ReadLine())
        
let print  x =
    Console.WriteLine "digraph program_graph {rankdir=LR; \nnode [shape = circle]; q▷; \nnode [shape = doublecircle]; q◀; \nnode [shape = circle]     "       
    let rec print' list endpoint covered =
        match list with
        | [] -> Console.WriteLine("// " + "The statement \"the graph is covered\" is: " + string (covered))
        | (x',y',(z':string),covered')::xs -> Console.WriteLine (prettyprinterPretext x' y' endpoint z')
                                              print' xs endpoint (covered' && covered)
    let endpoint= max2 x
    print' x endpoint true
    Console.WriteLine "}"

(* let parseAll =
   Console.Write("Enter your favorite GCL expression: ")
   let string = parse (readAll (Console.ReadLine()))
   Console.WriteLine string
   Console.WriteLine (prettyprintCommand string)
 *)
let rec pow x n = match n with
                   | 0 -> 1
                   | n when n>0 -> x*(pow x (n-1))
                   | n -> failwith "n should not be negative"

let rec evalExpression expr vars arrs = 
    match expr with
    | Num (x) -> x
    | X (s) -> let var = getValueVars s vars
               match var with
               | None -> failwith "Variable not defined"
               | Some(x) -> x
    | Arr (s, i) -> let arr = getValueArrs s (evalExpression i vars arrs) arrs
                    match arr with
                    | None -> failwith "Array variable not defined"
                    | Some(x) -> x
    | TimesExpr (e1, e2) -> (evalExpression e1 vars arrs) * (evalExpression e2 vars arrs)
    | DivExpr  (e1, e2) -> (evalExpression e1 vars arrs) / (evalExpression e2 vars arrs)
    | PlusExpr  (e1, e2) -> (evalExpression e1 vars arrs) + (evalExpression e2 vars arrs)
    | MinusExpr  (e1, e2) -> (evalExpression e1 vars arrs) - (evalExpression e2 vars arrs)
    | PowExpr  (e1, e2) -> pow (evalExpression e1 vars arrs) (evalExpression e2 vars arrs)
    | UPlusExpr  (e1) -> (evalExpression e1 vars arrs)
    | UMinusExpr  (e1) -> -(evalExpression e1 vars arrs)
    | APar (e1) -> (evalExpression e1 vars arrs)

and assignVar x y vars = match vars with
                            | [] -> [(x, y)]
                            | (x', y')::xs when x' = x -> (x', y)::xs
                            | (x', y')::xs -> (x', y')::(assignVar x y xs)

and assignArr x i y vars arrs = match arrs with
                                    | [] -> [((x, i), y)]
                                    | ((x', i'), y')::xs when x'=x && i'=i -> ((x', i'), y)::xs
                                    | ((x', i'), y')::xs -> ((x', i'), y')::(assignArr x i y vars xs)


and getValueVars x vars = match vars with
                            | [] -> None
                            | (x', y')::_ when x' = x -> Some y'
                            | _::xs -> getValueVars x xs

and getValueArrs x i arrs = match arrs with
                             | [] -> None
                             | ((x', i'), y')::_ when (x', i') = (x, i)-> Some y'
                             | _::xs -> getValueArrs x i xs

let rec evaluateCommand c vars arrs (evaluationsteps:EvalList) stepcount maxcount = 
   if stepcount<maxcount then
        match c with
            | Assign (s, a) ->( (assignVar s (evalExpression a vars arrs) vars), arrs, (((s+":="+prettyprintExp a),(vars,arrs))::evaluationsteps) )
            | AssignArr (s, i, a) ->  (vars, (assignArr s (evalExpression i vars arrs) (evalExpression a vars arrs) vars arrs),( (prettyprintExp(Arr(s,i))+":="+(prettyprintExp a),(vars,arrs)) ::evaluationsteps) )
            | CommandList (c1, c2) -> let (vars1, arrs1,evaluationsteps1) = evaluateCommand c1 vars arrs evaluationsteps (stepcount+1) maxcount
                                      evaluateCommand c2 vars1 arrs1 evaluationsteps1 (stepcount+1) maxcount
            | Ifstate g -> evalIfGuard g vars arrs (evaluationsteps) (stepcount+1) maxcount
            | Dostate g -> let (bool,(vars1,arrs1,list1)) = evalDoGuard g vars arrs (evaluationsteps) (stepcount+1) maxcount []
                           let boolskip = (combinedguard(bool) ,(vars1,arrs1))
                           (vars1,arrs1,(boolskip::list1))

            | Skip -> (vars, arrs,("skip",(vars,arrs))::evaluationsteps)
            | PredCommand (p1, c, p2) -> 
                evaluateCommand c vars arrs evaluationsteps stepcount maxcount
    else
        Console.WriteLine (string(stepcount))
        (vars,arrs,evaluationsteps)



and evalIfGuard g vars arrs evaluationsteps stepcount maxcount =
    match g with
    | Bfunc (b, c) when (evalBool b vars arrs) -> evaluateCommand c vars arrs ((prettyprintBool(b),(vars,arrs))::evaluationsteps) (stepcount+1) maxcount
    | Twoguard (g1, g2) -> let (vars1, arrs1,evaluationsteps1) = (evalIfGuard g1 vars arrs evaluationsteps  (stepcount+1) maxcount)
                           evalIfGuard g2 vars1 arrs1 evaluationsteps1  (stepcount+1) maxcount
    | _ -> (vars, arrs,evaluationsteps)    
and evalDoGuard g vars arrs evaluationsteps stepcount maxcount boollist=
    
        match g with
        | Bfunc (b, c) -> if (evalBool b vars arrs)
                          then let (resultvar,resultarr,resultevalsteps) = evaluateCommand c vars arrs ((prettyprintBool(b),(vars,arrs))::evaluationsteps)  (stepcount+1) maxcount
                               if stepcount<maxcount then
                                    evalDoGuard g resultvar resultarr (("skip",(resultvar, resultarr))::resultevalsteps)  (stepcount+1) maxcount boollist
                               else 
                                     (["true"],(vars,arrs,evaluationsteps))    
                          else (prettyprintBool(Not(b))::boollist,(vars,arrs,evaluationsteps) ) 
        | Twoguard (g1, g2) -> let (boollist1,(vars1, arrs1,evaluationsteps1)) = (evalDoGuard g1 vars arrs evaluationsteps)  (stepcount+1) maxcount boollist
                               evalDoGuard g2 vars1 arrs1 (evaluationsteps1)  (stepcount+1) maxcount boollist1
                           
    

and evalBool b vars arrs  =
    match b with
    | True -> true
    | False -> false
    | And (b1, b2) when (evalBool b1 vars arrs) -> (evalBool b2 vars arrs )
    | DAnd (b1, b2) -> (evalBool b1 vars arrs) && (evalBool b2 vars arrs )
    | Or (b1, b2) when (evalBool b1 vars arrs )= (true || false) -> (evalBool b1 vars arrs ) ||(evalBool b2 vars arrs )
    | DOr (b1, b2) -> (evalBool b1 vars arrs ) || (evalBool b2 vars arrs )
    | Not (b1) -> not (evalBool b1 vars arrs )
    | Equal (aexpr1 , aexpr2) when  (evalExpression aexpr1 vars arrs) = (evalExpression aexpr2 vars arrs) -> true
    | NotEqual (aexpr1, aexpr2) when ( not ((evalExpression aexpr1 vars arrs) = (evalExpression aexpr2 vars arrs))) -> true
    | Geq (aexpr1 , aexpr2) when  (evalExpression aexpr1 vars arrs) >= (evalExpression aexpr2 vars arrs) -> true
    | Gt (aexpr1 , aexpr2) when (evalExpression aexpr1 vars arrs) > (evalExpression aexpr2 vars arrs) -> true
    | Lt (aexpr1 , aexpr2) when  (evalExpression aexpr1 vars arrs) < (evalExpression aexpr2 vars arrs) -> true
    | Leq(aexpr1 , aexpr2) when  (evalExpression aexpr1 vars arrs) <= (evalExpression aexpr2 vars arrs) -> true
    | ParB (b1) -> evalBool b1 vars arrs
    | _ -> false
and evaluatePred p vars arrs = 
    match p with
    | PBool b -> evalBool b vars arrs
   // | Pexpr a -> evalExpression a vars arrs


let rec findInGraph label graph currentpos=
    match graph with 
    | [] -> let string = failwith ("Stuck at node:" + string(currentpos))
            (string,currentpos)
    | (x',y',z')::xs when (String.Equals (label, z') && currentpos=x') -> ((prettyprinterPretext x' y' (-1) z'),y')
    | _::xs -> findInGraph label xs currentpos

let rec printEvaliationList (evalList:EvalList) resultProgramGraph currentpos=
    match evalList with
    | [] -> Console.WriteLine ""
    | (x,(vars,arrs))::xs -> let (string1,nextpos) = findInGraph x resultProgramGraph currentpos
                            
                             Console.Write (string1)
                             Console.Write "      "
                             Console.Write vars
                             Console.Write "      "
                             Console.WriteLine arrs
                             printEvaliationList xs resultProgramGraph nextpos

let checkCommandType c =
    match c with
    | PredCommand (_, _, _) -> true
    | _ -> false

let run =
    Console.WriteLine("Enter initial variables: ")
    let (initVar,initArr,initeval) = evaluateCommand (parse (Console.ReadLine())) [] [] [] 0 50
    Console.WriteLine((initVar,initArr))
    Console.WriteLine ""
    Console.WriteLine("Enter your favorite GCL expression: ")
    let commands = parse (readAll (Console.ReadLine()) + "\n") // parse input
    Console.WriteLine (commands) // print AST
    Console.WriteLine ""
    Console.WriteLine (prettyprintCommand commands) // print result parsed back
    Console.WriteLine "" 
    //print program graph
    let result =pgc commands 1 1 [] 0 50 false
    let endpoint = max2(result)  //last node used  
    print (((0,1,"begin", checkCommandType commands)::result)@ [(endpoint,endpoint+1,"end", true)] ) // generates beginning node and endnode
    Console.WriteLine "" 
    //let  (vars, arrs,evaluationsteps:EvalList) = evaluateCommand commands initVar initArr [] 0 50
    //printEvaliationList (List.rev evaluationsteps) result 1 // print the steps taken to reach the final values
    //Console.WriteLine "" 
    //Console.Write "Final variable values: "
    //Console.WriteLine((vars,arrs))    // prints the final values 
let rec domP q p =  
    match p with
    | [] -> false
    | x::xs when x=q -> true
    | x::xs -> domP q xs

let rec build pg p spf=
    match pg with 
    | [] -> spf
    | q::qs when (domP q p)  -> build qs p (q::spf)
    | q::qs -> build qs p spf   


// For assignment 5
let sign a = if a > 0 then '+' else
                                 if a=0 then '0' else '-'
let signx x  vars = let result = (getValueVars x vars)   
                    match result with
                    | Some x -> sign x
                    | None -> failwith "unknown variable"      

let signevalminus x y = 
    match x, y with 
    | x,y when (x='-' && y='-') -> set ['-';'0';'+']  
    | x,y when (x='-' && y='0') -> set ['-']
    | x,y when (x='-' && y='+') -> set ['-'] 
    | x,y when (x='0' && y='-') -> set ['+']  
    | x,y when (x='0' && y='0') -> set ['0']
    | x,y when (x='0' && y='+') -> set ['-'] 
    | x,y when (x='+' && y='-') -> set ['+']  
    | x,y when (x='+' && y='0') -> set ['+']
    | x,y when (x='+' && y='+') -> set ['-';'0';'+']

let singevalDiv x y =
    match x,y with 
    | x,y when (x='-' && y='-') -> set ['+']  
    | x,y when (x='-' && y='0') -> set []
    | x,y when (x='-' && y='+') -> set ['-'] 
    | x,y when (x='0' && y='-') -> set ['0']  
    | x,y when (x='0' && y='0') -> set []
    | x,y when (x='0' && y='+') -> set ['0'] 
    | x,y when (x='+' && y='-') -> set ['-']  
    | x,y when (x='+' && y='0') -> set []
    | x,y when (x='+' && y='+') -> set ['+']
let rec helper x sety signfunction= 
        match sety with 
        | sety' when Set.isEmpty sety' -> set []
        | sety' ->  let element = sety'.MinimumElement
                    Set.union (helper x (sety'.Remove element) signfunction) (signfunction x element)

let rec signevalSets setx sety signfunction =
   match setx with 
   | setx' when Set.isEmpty setx' -> set []
   | setx' ->  let element = setx'.MinimumElement
               Set.union  (signevalSets (setx'.Remove element) sety signfunction)  (helper element sety signfunction)

