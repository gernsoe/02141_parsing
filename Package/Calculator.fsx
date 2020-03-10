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

let rec eval e =
  match e with
    | AssignX(_) -> "God nok"
    | AssignA(_) -> "God nok"
    | Dood(_) -> "God nok"
    | Iffi(_) -> "God nok"
    | Next (_) -> "God nok"
    | Skip(_) -> "God nok"
    //| Num(x) -> x
 


// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        //try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %s" (eval(e))
        compute n
        //with err -> compute (n-1)

// Start interacting with the user
compute 3
