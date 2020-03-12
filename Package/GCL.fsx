// This script implements our interactive calculator

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

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

(* let rec eval e =
  match e with
    | AssignX(x) -> eval x
    | AssignA(x) -> eval x
    | Dood(x) -> eval x
    | Iffi(x) -> eval x
    | Next (x,y) -> eval x eval y
    | Skip -> ""
    | Condition(x,y) -> eval x eval y
    | ElseIfExpr(x,y) -> eval x eval y
    | N(x) -> ""
    | X(x) -> ""
    | 

 *)

// We
let parse lexbuf =
    // translate string into a buffer of characters
    // translate the buffer into a stream of tokens and parse them
    let res =   try
                GCLParser.start GCLLexer.tokenize lexbuf
                with err -> 
                let pos = lexbuf.EndPos
                let hintPos = sprintf "[Line: %d, Character: %d]" (pos.Line) (pos.Column)
                let lastExpression = String(lexbuf.Lexeme)
                let message = sprintf "%s | Grammer not recognize at: %s – Look at the expression: '%s'|" err.Message hintPos lastExpression
                printfn "%s" message     //throw exception here
                exit 1
    // return the result of parsing (i.e. value of type "expr")
    res
    
let test1 = "if !true && false -> x := 5; x:=1 fi"
    
let test2 = "do true -> continue od"

let test3 = "y:=2;
             do false -> y:= x*y;
                       x:=x-1
             od"
             
let test4 = "y:=2;
             do x>0 -> y:= x*y;
                       x:=x-1 
                          "

let test5 = "x:=2;
            if true && !false -> x:= (((x*2)/2)+2-2)^2 
            [] x != 2 -> x:= 2; A[x] := 1 fi"

// We implement here the function that interacts with the user
let rec compute n =
        printf "Enter an expression: "
        // We parse the input string
        let lexbuf = LexBuffer<_>.FromString (Console.ReadLine())

        let expression = parse lexbuf 
        // and print the result of evaluating it
        printfn "God nok" 

// Start interacting with the user

compute test5

