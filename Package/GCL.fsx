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
#load "GCLCompiler.fsx"
open GCLCompiler

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
 

// Start interacting with the user
printf "Enter an expression: "
let lexbuf = LexBuffer<_>.FromString (Console.ReadLine());
let expression = parse lexbuf
compute expression
