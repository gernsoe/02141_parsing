// This script implements our test cases

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
    
// Expected to be recognized
let test1 = "if !true && false -> x := 5; x:=1 fi"

// Expected to be recognized
let test2 = "do true -> continue od"

// Expected to be recognized
let test3 = "y:=2;
             do false -> y:= x*y;
                       x:=x-1
             od"
        
// Error expected
let test4 = "y:=2;
             do x>0 -> y:= x*y;
                       x:=x-1 
                          "

// Expected to be recognized
let test5 = "x:=2;
            if true && !false -> x:= (((x*2)/2)+2-2)^2 
            [] x != 2 -> x:= 2; A[x] := 1 fi"

// We implement here the function that interacts with the user
let rec compute n =
        printf "Enter an expression: "
        // translate string into a buffer of characters
        let lexbuf = LexBuffer<_>.FromString (n)

        // Gonna be used for a future pretty printer functionality
        let expression = parse lexbuf 
        // and print the result of evaluating it
        printfn "Grammar recognized" 

// Starting tests
compute test1
compute test2
compute test3
compute test4
compute test5

