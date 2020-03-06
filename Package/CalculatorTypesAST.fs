
// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module GCL_AST

type x =
    | xString of string
    | xChar of char

type a =
    | n of int
    | X of x
    | A of array<a>
    | PlusExpr of (a * a)
    | MinusExpr of (a * a)
    | TimesExpr of (a * a)
    | DivExpr of (a * a)
    | UMinusExpr of (a)
    | PowExpr of (a * a)

type b =
    | True of bool
    | False of bool
    | And1Expr of (b * b)
    | Or1Expr of (b * b)
    | And2Expr of (b * b)
    | Or2Expr of (b * b)
    | NotExpr of b
    | EquExpr of (a * a)
    | NequExpr of (a * a)
    | Gt of (a * a)
    | Ge of (a * a)
    | Lt of (a * a)
    | Le of (a * a)

type C =
  | AssignX of (x * a)
  | AssignA of (a * a)
  | Skip of string
  | Next of (C * C)
  | Iffi of GC
  | Dood of GC
and GC =
  | Condition of (b * C)
  | ElseIfExpr of (GC * GC)