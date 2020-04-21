open System.IO
// This script implements our interactive parser

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.dll"
#load "GCLTypesAST.fs"
open GCLTypesAST
open FSharp.Text.Lexing
open System
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer
#load "GCLCompiler.fs"
open GCLCompiler

type Expression =
    | BExpression of b
    | CExpression of C

let nodeShape = 
    ["digraph program_graph {rankdir=LR; \n" +
     "node [shape = circle]; q▷; \n" +
     "node [shape = doublecircle]; q◀; \n" +
     "node [shape = circle]"];

let rec aEval a =
   match a with
    | N(x) -> (string) x
    | X(s) -> s
    | A(s, x) -> s + "[" + aEval(x) + "]"
    | TimesExpr(x,y) -> aEval(x) +  "*" + aEval (y)
    | DivExpr(x,y) -> aEval(x) + "/" + aEval (y)
    | PlusExpr(x,y) -> aEval(x) + "+" + aEval (y)
    | MinusExpr(x,y) -> aEval(x) + "-" + aEval (y)
    | PowExpr(x,y) -> aEval(x) + "^" + aEval (y)
    | UMinusExpr(x) -> "-" + aEval(x)
    | ParaA(x) -> "(" + aEval(x) + ")"
and bEval b = 
    match b with
       | True -> "true"
       | False -> "false"
       | And1Expr(x,y) -> bEval(x) +  "&" + bEval (y)
       | Or1Expr(x,y) -> bEval(x) + "|" + bEval (y)
       | And2Expr(x,y) -> bEval(x) + "&&" + bEval (y)
       | Or2Expr(x,y) -> bEval(x) + "||" + bEval (y)
       | NotExpr(x) -> "!(" + bEval(x) + ")"
       | EqExpr(x,y) -> aEval(x) + "=" + aEval (y)
       | NeqExpr(x,y) -> aEval(x) + "!=" + aEval (y)
       | Gt(x,y) -> aEval(x) + ">" + aEval (y)
       | Ge(x,y) -> aEval(x) + ">=" + aEval (y)
       | Lt(x,y) -> aEval(x) + "<" + aEval (y)
       | Le(x,y) -> aEval(x) + "<=" + aEval (y)
       | ParaB(x) -> "(" + bEval(x) + ")"

let rec dEval gc = 
    match gc with 
        | Condition(b, c) -> b
        | ElseIfExpr(gc1, gc2) -> And1Expr(dEval(gc1),dEval(gc2))

let rec expEval (exp: Expression) =
    match exp with
        | CExpression(AssignX(s,a)) -> s + ":=" + aEval(a)
        | CExpression(AssignA(s, a1,a2)) -> aEval(a1) + ":=" + aEval(a2)
        | CExpression(Skip) -> "skip"
        | BExpression b -> bEval(b)
        | _ -> "";

let mutable counter = 0;      
let rec cEval start slut c =
    match c with
        | AssignX(s, a) -> "q" + start + " -> q" + slut + "[label = \"" + s + ":=" + aEval(a) + "\"];"
        | AssignA(s, a1, a2) -> "q" + start + " -> q" + slut + "[label = \"" + aEval(a1) + ":=" + aEval(a2) + "\"];"
        | Skip -> "q" + start + " -> q" + slut + "[label = \"skip\"];"
        | Next(c1, c2) -> counter <- counter + 1
                          cEval start (counter.ToString()) c1  + "\n" + cEval (counter.ToString()) slut c2
        | Iffi(x) -> gcEval start slut x
        | Dood(x) -> gcEval start start x + "\n" + "q" + start + " -> q" + slut + "[label = \"!(" + bEval(dEval(x)) + ")\"];"
and gcEval start slut gc =
    match gc with
        | Condition(b, c) -> counter <- counter + 1
                             match (start, slut) with
                                | ("▷",_) -> ("q▷ -> q" + counter.ToString() + "[label = \"" + bEval(b) + "\"]; \n" + cEval (counter.ToString()) slut c )
                                | (_,_) -> ("q" + start + " -> q" + counter.ToString() + "[label = \"" + bEval(b) + "\"]; \n" + cEval (counter.ToString()) slut c )
        | ElseIfExpr(gc1, gc2) -> gcEval start slut gc1 + "\n" + gcEval start slut gc2

let mutable fix = False;
let rec cDEval start slut c d =
    match c with
        | CExpression(AssignX(s, a)) -> [("q" + start, CExpression(AssignX(s,a)) , "q" + slut)] //  "q" + start + " -> q" + slut + "[label = \"" + s + ":=" + aEval(a) + "\"];"
        | CExpression(AssignA(s, a1, a2)) -> [("q" + start, CExpression(AssignA(s, a1,a2)), "q" + slut)]//"q" + start + " -> q" + slut + "[label = \"" + aEval(a1) + ":=" + aEval(a2) + "\"];"
        | CExpression(Skip) -> [("q" + start, CExpression(Skip), "q" + slut)] // "q" + start + " -> q" + slut + "[label = \"skip\"];"
        | CExpression(Next(c1, c2)) -> counter <- counter + 1
                                       cDEval start (counter.ToString()) (CExpression(c1)) d @ cDEval (counter.ToString()) slut (CExpression(c2)) d
        | CExpression(Iffi(x)) -> gcDEval start slut x d
        | CExpression(Dood(x)) -> gcDEval start start x d @ [("q"+start, BExpression(NotExpr(Or1Expr(dEval(x),d))) , "q"+slut )]//+ "\n" + "q" + start + " -> q" + slut + "[label = \"!(" + dEval(x) + "|" + d + ")\"];"
        | _ -> [];

and gcDEval start slut gc d =
    match gc with
        | Condition(b, c) -> fix <- Or1Expr(b,d)
                             counter <- counter + 1
                             match (start, slut) with
                                | ("▷",_) -> ("q▷", BExpression(And1Expr(ParaB(b),NotExpr(d))) ,"q"+counter.ToString())::cDEval (counter.ToString()) slut (CExpression(c)) d //("q▷ -> q" + counter.ToString() + "[label = \"(" + bEval(b) + ")&!(" + d +  ")\"]; \n" + cDEval (counter.ToString()) slut c d) 
                                | (_,_) ->  ("q"+start, BExpression(And1Expr(ParaB(b),NotExpr(d))) ,"q"+counter.ToString())::cDEval (counter.ToString()) slut (CExpression(c)) d //("q" + start + " -> q" + counter.ToString() + "[label = \"(" + bEval(b) + ")&!(" + d + ")\"]; \n" + cDEval (counter.ToString()) slut c d)
        | ElseIfExpr(gc1, gc2) -> gcDEval start slut gc1 d @ gcDEval start slut gc2 (fix)


// We implement here the function that interacts with the user
let compute expression =

        let edgeList = cDEval "▷" "◀" (CExpression expression) False
        let edgeStringList = List.map(fun (qstart,act,qslut) -> (qstart, expEval act, qslut)) edgeList 
        let edgeString = List.map (fun(qstart,act,qslut) -> qstart + " -> " + qslut + "[label = \"" + act + "\"];" ) edgeStringList


        // and print the result of evaluating it
        printfn "Grammar recognized" 
        
        //File.WriteAllText("NFA.gv", nodeShape + "\n" + (cEval "▷" "◀" expression + "\n}"))
        counter <- 0
        File.WriteAllLines("DFA.gv", nodeShape @ edgeString @ ["}"])

        edgeList

let parse lexbuf =
    // translate the buffer into a stream of tokens and parse them
    let res =   try
                GCLParser.start GCLLexer.tokenize lexbuf
                with err -> 
                // Get last position before lexer error
                let pos = lexbuf.EndPos 

                // Show line number and character position
                let exactPos = sprintf "[Line: %d, Character: %d]" (pos.Line) (pos.Column) 

                // Get the expression that caused the error
                let lastExpression = String(lexbuf.Lexeme)  

                // Print the error message
                let message = sprintf "%s | Grammer not recognize at: %s \nLook at the expression: '%s'" err.Message exactPos lastExpression 
                printfn "%s" message     //throw exception here
                exit 1
    // return the result of parsing (i.e. value of type "expr")
    res;;


let rec interpret_aEval a (mem:Map<string,int[]>) =
    match a with
       | N(x) -> x
       | X(s) -> let found = mem.TryFind s
                 match found with
                 | Some x -> x.[0]
                 | None -> 0
       | A(s, x) -> let found = mem.TryFind s
                    match found with
                    | Some a -> a.[interpret_aEval x mem]
                    | None -> 0
       | TimesExpr(x,y) -> (interpret_aEval x mem) * (interpret_aEval y mem)
       | DivExpr(x,y) -> (interpret_aEval x mem) / (interpret_aEval y mem)
       | PlusExpr(x,y) -> (interpret_aEval x mem) + (interpret_aEval y mem)
       | MinusExpr(x,y) -> (interpret_aEval x mem) - (interpret_aEval y mem)
       | PowExpr(x,y) -> pown (interpret_aEval x mem) (interpret_aEval y mem)
       | UMinusExpr(x) -> -(interpret_aEval x mem)
       | ParaA(x) -> (interpret_aEval x mem)
and interpret_bEval b (mem:Map<string,int[]>) = 
    match b with
       | True -> true
       | False -> false
       | And1Expr(x,y) -> let b1 = interpret_bEval x mem
                          let b2 = interpret_bEval y mem
                          b1 && b2
       | Or1Expr(x,y) ->  let b1 = interpret_bEval x mem
                          let b2 = interpret_bEval y mem
                          b1 || b2
       | And2Expr(x,y) -> (interpret_bEval x mem) && (interpret_bEval y mem)
       | Or2Expr(x,y) -> (interpret_bEval x mem) || (interpret_bEval y mem)
       | NotExpr(x) -> not (interpret_bEval x mem)
       | EqExpr(x,y) -> (interpret_aEval x mem) = (interpret_aEval y mem)
       | NeqExpr(x,y) -> (interpret_aEval x mem) <> (interpret_aEval y mem)
       | Gt(x,y) -> (interpret_aEval x mem) > (interpret_aEval y mem)
       | Ge(x,y) -> (interpret_aEval x mem) >= (interpret_aEval y mem)
       | Lt(x,y) -> (interpret_aEval x mem) < (interpret_aEval y mem)
       | Le(x,y) -> (interpret_aEval x mem) <= (interpret_aEval y mem)
       | ParaB(x) -> (interpret_bEval x mem);;

let memory = Map.empty<string, int[]>;;

// Returns true if the input edge can be chosen
let checkAct (mem:Map<string,int[]>) edge =
    match edge with
    | (_,CExpression(act),_) -> true
    | (_,BExpression(act),_) -> interpret_bEval act mem;;


// Returns the edges which can be chosen
let possibleActs mem edges = List.filter (checkAct mem) edges;;

// Write assignemnts into the memory
let evaluateAct act mem = 
    match act with 
    | AssignX(x,y) -> Map.add x ([|interpret_aEval y mem|]) mem
    | AssignA(s,x,y) -> let found = mem.TryFind s
                        match found with
                        | Some a -> Map.add s (a |> Array.mapi(fun i v -> if i = interpret_aEval x mem then interpret_aEval y mem else v)) mem
                        | None -> mem
    |_ -> mem
    ;;

let rec printMem node memList =
    //let status = List.find(fun(x,y) -> x = "status") |> List.map(fun(x,y) -> )   
    match memList with
    |(k,v)::tail when k = "status" -> match v with
                                      | [|0|] -> "status: Stuck\nNode: " + node + "\n" + (printMem node tail)
                                      | [|1|] -> "status: Terminated \nNode: " + node + "\n" + (printMem node tail)
                                      | _ -> ""
    |(k,v)::tail -> k + ": " + (string) v.[0] + "\n" + (printMem node tail)
    |(k,v)::[] when k = "status" -> match v with
                                    | [|0|] -> "status: Stuck at "
                                    | [|1|] -> "status: Terminated \n"
                                    | _ -> ""
    |(k,v)::[] -> k + ": " + (string) v.[0]
    | _ -> ""

let mutable endNode = "";;
// Walk the programgraph by chosing an edge from the node given as input to the function
let rec interpret edgelist node mem =
    if node = "q◀" then  endNode <- node
                         Map.add "status" [|1|] mem 
                       //|> Map.toList |> printMem node |> sprintf "%s"
    else 
    let edges = List.filter(fun (qstart, act, qslut) -> qstart = node) edgelist
    let possibleEdge = possibleActs mem edges
    match possibleEdge with
    | (_,CExpression(act),qslut)::[] -> evaluateAct act mem |> interpret edgelist qslut
    | (_,BExpression(act), qslut)::[] -> interpret edgelist qslut mem
    | _ -> endNode <- node
           Map.add "status" [|0|] mem //|> Map.toList |> printMem node |> sprintf "%s"
    ;;



let rec initializeVariables userInput (mem:Map<string,int[]>) =
    match userInput with
    | AssignX(x,y) -> Map.add x ([|interpret_aEval y mem|]) mem 
    | Next(x,y) -> initializeVariables x mem |> 
                   initializeVariables y
    | _ -> mem;;

// Sign Analyser
let multiDivTable = Map.ofList([(('+','+'),['+']);(('-','-'),['+']);(('0','0'),['0']);(('+','0'),['0']);(('-','0'),['0']);
                            (('0','+'),['0']);(('0','-'),['0']);(('-','+'),['-']);(('+','-'),['-'])])

let plusTable = Map.ofList([(('+','+'),['+']);(('-','-'),['-']);(('0','0'),['0']);(('+','0'),['+']);(('-','0'),['-']);
                            (('0','+'),['+']);(('0','-'),['-']);(('-','+'),['-';'0';'+']);(('+','-'),['-';'0';'+'])])

let minusTable = Map.ofList([(('+','+'),['-']);(('-','-'),['-';'0';'+']);(('0','0'),['0']);(('+','0'),['+']);(('-','0'),['-']);
                            (('0','+'),['-']);(('0','-'),['+']);(('-','+'),['-']);(('+','-'),['+'])])
                                                   
let powTable = Map.ofList([(('+','+'),['+']);(('-','-'),['-';'+']);(('0','0'),['+']);(('+','0'),['+']);(('-','0'),['+']);
                            (('0','+'),['0']);(('0','-'),['0']);(('-','+'),['-';'+']);(('+','-'),['+'])])

let andTable = Map.ofList([(('t','t'),['t']);(('f','f'),['f']);(('t','f'),['f']);(('f','t'),['f'])])

let orTable = Map.ofList([(('t','t'),['t']);(('f','f'),['f']);(('t','f'),['t']);(('f','t'),['t'])])

let eqTable = Map.ofList([(('+','+'),['t';'f']);(('-','-'),['t';'f']);(('0','0'),['t']);(('+','0'),['f']);(('-','0'),['f']);
                            (('0','+'),['f']);(('0','-'),['f']);(('-','+'),['f']);(('+','-'),['f'])])

let neqTable = Map.ofList([(('+','+'),['t';'f']);(('-','-'),['t';'f']);(('0','0'),['f']);(('+','0'),['t']);(('-','0'),['t']);
                            (('0','+'),['t']);(('0','-'),['t']);(('-','+'),['t']);(('+','-'),['t'])])

let gtTable = Map.ofList([(('+','+'),['t';'f']);(('-','-'),['t';'f']);(('0','0'),['f']);(('+','0'),['t']);(('-','0'),['f']);
                            (('0','+'),['f']);(('0','-'),['t']);(('-','+'),['f']);(('+','-'),['t'])])

let geTable = Map.ofList([(('+','+'),['t';'f']);(('-','-'),['t';'f']);(('0','0'),['t']);(('+','0'),['t']);(('-','0'),['f']);
                            (('0','+'),['f']);(('0','-'),['t']);(('-','+'),['f']);(('+','-'),['t'])])
                            
// Start interacting with the user
printfn "Enter an expression: ";;
let lexbuf = LexBuffer<_>.FromString (Console.ReadLine());;
let expression = parse lexbuf;;
let edgeList = compute expression;;
printfn "Initialize your variables in this format x:=2;y:=0 ";;
let interpretLexbuf = LexBuffer<_>.FromString (Console.ReadLine())
let interpretExpression = parse interpretLexbuf
let initializedMemory = initializeVariables (interpretExpression) memory;;
let mem = interpret edgeList "q▷" initializedMemory;;
let sortedList2 = List.sortBy (fun (x,y) -> x = "status") (Map.toList mem) |> List.rev ;;
printfn "%s" (printMem endNode (sortedList2))
