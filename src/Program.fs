module Program

open System
open REPL
open Parser
open Interpreter
open Ast
open System.IO

[<EntryPoint>]
let main args =
    match args with
    | [| |] ->
        printfn "MiniFuncLang REPL. Type 'quit' to exit."
        loop (initialEnv())
    | [| filePath |] ->
        if File.Exists filePath then
            let code = File.ReadAllText filePath
            try
                let ast = parse code
                let v   = eval ast (initialEnv()) |> force
                printfn "%A" v
            with
            | ParseError msg   -> printfn "Parse error: %s" msg
            | RuntimeError msg -> printfn "Runtime error: %s" msg
            | ex               -> printfn "Unknown error: %s" ex.Message
        else
            printfn "File not found: %s" filePath
    | _ ->
        printfn "Usage:\n  Without args: REPL mode\n  With file path: execute script"

    0
