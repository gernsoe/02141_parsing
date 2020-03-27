module GCLCompiler
open System

#load "GCLTypesAST.fs"
open GCLTypesAST

let nodeShape = 
    "digraph program_graph {rankdir=LR; \n" +
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
        | Condition(b, c) -> bEval(b)
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
        | Dood(x) -> gcEval start start x + "\n" + "q" + start + " -> q" + slut + "[label = \"!(" + dEval(x) + ")\"];"
and gcEval start slut gc =
    match gc with
        | Condition(b, c) -> counter <- counter + 1
                             match (start, slut) with
                                | ("▷",_) -> ("q▷ -> q" + counter.ToString() + "[label = \"" + bEval(b) + "\"]; \n" + cEval (counter.ToString()) slut c )
                                | (_,_) -> ("q" + start + " -> q" + counter.ToString() + "[label = \"" + bEval(b) + "\"]; \n" + cEval (counter.ToString()) slut c )
        | ElseIfExpr(gc1, gc2) -> gcEval start slut gc1 + "\n" + gcEval start slut gc2

let mutable fix = "";
let rec cDEval start slut c d =
    match c with
        | AssignX(s, a) -> "q" + start + " -> q" + slut + "[label = \"" + s + ":=" + aEval(a) + "\"];"
        | AssignA(a1, a2) -> "q" + start + " -> q" + slut + "[label = \"" + aEval(a1) + ":=" + aEval(a2) + "\"];"
        | Skip -> "q" + start + " -> q" + slut + "[label = \"skip\"];"
        | Next(c1, c2) -> counter <- counter + 1
                          cDEval start (counter.ToString()) c1 d + "\n" + cDEval (counter.ToString()) slut c2 d
        | Iffi(x) -> gcDEval start slut x d
        | Dood(x) -> gcDEval start start x d + "\n" + "q" + start + " -> q" + slut + "[label = \"!(" + dEval(x) + "|" + d + ")\"];"
and gcDEval start slut gc d =
    match gc with
        | Condition(b, c) -> fix <- bEval(b) + "|" + d
                             counter <- counter + 1
                             match (start, slut) with
                                | ("▷",_) -> ("q▷ -> q" + counter.ToString() + "[label = \"(" + bEval(b) + ")&!(" + d +  ")\"]; \n" + cDEval (counter.ToString()) slut c d) 
                                | (_,_) -> ("q" + start + " -> q" + counter.ToString() + "[label = \"(" + bEval(b) + ")&!(" + d + ")\"]; \n" + cDEval (counter.ToString()) slut c d)
        | ElseIfExpr(gc1, gc2) -> gcDEval start slut gc1 d + "\n" + gcDEval start slut gc2 (fix)