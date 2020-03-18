// This script implements our interactive parser

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

let nodeShape = 
    "diagraph program_graph {rankdir=LR; \n" +
    "node [shape = circle]; q▷; \n" +
    "node [shape = doublecircle]; q◀; \n" +
    "node [shape = circle]"

let rec aEval a =
  match a with
    | N(x) -> (string) x
    | X(s) -> s
    | A(x) -> "A[" + aEval(x) + "]"
    | TimesExpr(x,y) -> aEval(x) +  "*" + aEval (y)
    | DivExpr(x,y) -> aEval(x) + "/" + aEval (y)
    | PlusExpr(x,y) -> aEval(x) + "+" + aEval (y)
    | MinusExpr(x,y) -> aEval(x) + "-" + aEval (y)
    | PowExpr(x,y) -> aEval(x) + "^" + aEval (y)
    | UMinusExpr(x) -> "-" + aEval(x)
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

let rec dEval gc = 
    match gc with 
        | Condition(b, c) -> bEval(NotExpr(b))
        | ElseIfExpr(gc1, gc2) -> dEval(gc1) + "&" + dEval(gc2)


let mutable counter = 0;      
let rec cEval start slut c =
    match c with
        | AssignX(s, a) -> "q" + start + " -> q" + slut + "[label = \"" + s + ":=" + aEval(a) + "\"];"
        | AssignA(a1, a2) -> "q" + start + " -> q" + slut + "[label = \"" + aEval(a1) + ":=" + aEval(a2) + "\"];"
        | Skip -> "q" + start + " -> q" + slut + "[label = \"skip\"];"
        | Next(c1, c2) -> counter <- counter + 1
                          cEval start (counter.ToString()) c1  + "\n" + cEval (counter.ToString()) slut c2
        | Iffi(x) -> gcEval start slut x
        | Dood(x) -> gcEval start start x + "\n" + "q" + start + " -> q" + slut + "[label = \"" + dEval(x) + "\"];"
and gcEval start slut gc =
    match gc with
        | Condition(b, c) -> counter <- counter + 1
                             match (start, slut) with
                                | ("▷",_) -> ("q▷ -> q" + counter.ToString() + "[label = \"" + bEval(b) + "\"]; \n" + cEval (counter.ToString()) slut c )
                                | (_,_) -> ("q" + start + " -> q" + counter.ToString() + "[label = \"" + bEval(b) + "\"]; \n" + cEval (counter.ToString()) slut c )
        | ElseIfExpr(gc1, gc2) -> gcEval start slut gc1 + "\n" + gcEval start slut gc2
       



// We
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
    res
    
// We implement here the function that interacts with the user
let rec compute =
        printf "Enter an expression: "
        // translate string into a buffer of characters
        let lexbuf = LexBuffer<_>.FromString (Console.ReadLine())

        // Gonna be used for a future pretty printer functionality
        let expression = parse lexbuf 
        // and print the result of evaluating it
        printfn "Grammar recognized" 

        printfn "%s" nodeShape 
        printfn "%s \n}" (cEval "▷" "◀" expression)

// Start interacting with the user
compute

