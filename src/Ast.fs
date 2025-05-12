module Ast

/// Наши выражения
type Expr =
  | IntLit    of int
  | BoolLit   of bool
  | StringLit of string
  | Var       of string
  | Lambda    of string * Expr
  | Apply     of Expr * Expr
  | If        of Expr * Expr * Expr
  | Let       of string * Expr * Expr
  | LetRec    of string * Expr * Expr
  | ListNil
  | ListCons  of Expr * Expr
  | Builtin   of string

/// Значения во время выполнения
type Value =
  | IntVal       of int
  | BoolVal      of bool
  | StringVal    of string
  | Closure      of string * Expr * Env ref
  | Thunk        of Expr * Env ref
  | ListVal      of Value list
  | BuiltinFunc  of (Value list -> Value)

and Env = Map<string, Value>
