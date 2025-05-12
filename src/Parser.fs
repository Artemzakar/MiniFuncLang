module Parser

open System
open Ast

exception ParseError of string

/// Сырые s-выражения
type SExpr =
  | Atom  of string
  | SList of SExpr list

/// Токенизация (строки, комментарии, скобки)
let tokenize (input: string) : string list =
    let buf = Text.StringBuilder()
    let tokens = ResizeArray<string>()
    let flush() =
        if buf.Length > 0 then
            tokens.Add(buf.ToString())
            buf.Clear() |> ignore

    let mutable inStr = false
    let mutable i = 0
    while i < input.Length do
        let c = input.[i]
        if inStr then
            buf.Append(c) |> ignore
            if c = '"' then inStr <- false; flush()
        else
            match c with
            | '"' ->
                flush()
                inStr <- true
                buf.Append(c) |> ignore
            | ';' ->
                flush()
                while i < input.Length && input.[i] <> '\n' do i <- i + 1
                i <- i - 1
            | '(' | ')' ->
                flush()
                tokens.Add(string c)
            | ' ' | '\t' | '\n' | '\r' ->
                flush()
            | _ ->
                buf.Append(c) |> ignore
        i <- i + 1
    flush()
    tokens |> Seq.toList

/// Преобразование токенов в сырой SExpr
let rec parseS (toks: string list) : SExpr * string list =
    match toks with
    | [] -> failwith "Unexpected end of input"
    | "(" :: rest ->
        let rec loop acc rem =
            match rem with
            | [] -> failwith "Unclosed ("
            | ")" :: rem' -> List.rev acc, rem'
            | _ ->
                let e, rem' = parseS rem
                loop (e::acc) rem'
        let lst, rem = loop [] rest
        SList lst, rem
    | ")" :: _ -> failwith "Unexpected )"
    | atom :: rest -> Atom atom, rest

/// Основная конвертация SExpr -> Expr
let rec toAst (sexpr: SExpr) : Expr =
    match sexpr with
    | Atom t ->
        if t.StartsWith("\"") && t.EndsWith("\"") && t.Length >= 2 then
            StringLit (t.Substring(1, t.Length - 2))
        else
            match Int32.TryParse t with
            | true, n -> IntLit n
            | _ ->
                match t with
                | "true"  -> BoolLit true
                | "false" -> BoolLit false
                | _       -> Var t

    | SList [Atom "if"; c; t; e] ->
        If(toAst c, toAst t, toAst e)

    | SList [Atom "let"; Atom name; value; body] ->
        Let(name, toAst value, toAst body)

    | SList [Atom "letrec"; Atom name; fundef; body] ->
        match fundef with
        | SList [Atom "lambda"; SList [Atom param]; expr] ->
            LetRec(name, Lambda(param, toAst expr), toAst body)
        | _ ->
            failwith "Invalid letrec syntax"

    | SList [Atom "lambda"; SList [Atom param]; body] ->
        Lambda(param, toAst body)

    // специальная форма list
    | SList (Atom "list" :: elems) ->
        // локальная рекурсивная функция для строительства вложенного ListCons
        let rec buildList items =
            match items with
            | []    -> ListNil
            | x::xs -> ListCons(toAst x, buildList xs)
        buildList elems

    // общее применение
    | SList (f :: args) ->
        args
        |> List.fold (fun acc arg -> Apply(acc, toAst arg)) (toAst f)

    | SList [] ->
        ListNil

/// Парсер на входе
let parse (input: string) : Expr =
    let sexpr, rem = parseS (tokenize input)
    if rem <> [] then
        let leftovers = String.concat " " rem
        raise (ParseError (sprintf "Extra tokens: %s" leftovers))
    toAst sexpr
