module REPL
open System
open Parser
open Interpreter
open Ast

let initialEnv () : Env ref =
    ref Map.empty

/// Проверяем, сбалансированы ли скобки в строке
let private balanced (s: string) =
    let cnt = s |> Seq.fold (fun acc c ->
                match c with
                | '(' -> acc + 1
                | ')' -> acc - 1
                | _   -> acc) 0
    cnt = 0

let rec loop env =
    let rec readExpr acc =
        if balanced acc then acc
        else
            printf "... "
            let next = Console.ReadLine()
            if isNull next then acc
            else readExpr (acc + "\n" + next)

    printf "> "
    let line = Console.ReadLine()
    if isNull line || line.Trim() = "quit" then
        printfn "Bye!"
    else
        let fullInput = readExpr line
        try
            let ast = parse fullInput
            let v   = eval ast env |> force
            printfn "%A" v
        with
        | ParseError msg   -> printfn "Parse error: %s" msg
        | RuntimeError msg -> printfn "Runtime error: %s" msg
        | ex               -> printfn "Unknown error: %s" ex.Message
        loop env
