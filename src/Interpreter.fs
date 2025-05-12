module Interpreter

open System.IO
open Ast

exception RuntimeError of string

/// Принудительно вычислить Thunk
let rec force (v: Value) : Value =
    match v with
    | Thunk(e, env) ->
        let v' = eval e env
        force v'
    | _ -> v

and eval (expr: Expr) (env: Env ref) : Value =
    let wrap1 name f =
        BuiltinFunc(fun args ->
            match args with
            | [a] -> f a
            | _   -> raise (RuntimeError $"Bad args to {name}"))

    let wrap2 name f =
        BuiltinFunc(fun args ->
            match args with
            | [a] ->
                BuiltinFunc(fun args2 ->
                    match args2 with
                    | [b] -> f a b
                    | _   -> raise (RuntimeError $"Bad args to {name}"))
            | _ -> raise (RuntimeError $"Bad args to {name}"))

    let builtins =
        Map.ofList [
            // арифметика
            "+", wrap2 "+" (fun a b ->
                    match a,b with IntVal x,IntVal y -> IntVal(x+y) | _-> raise (RuntimeError "+"))
            "-", wrap2 "-" (fun a b ->
                    match a,b with IntVal x,IntVal y -> IntVal(x-y) | _-> raise (RuntimeError "-"))
            "*", wrap2 "*" (fun a b ->
                    match a,b with IntVal x,IntVal y -> IntVal(x*y) | _-> raise (RuntimeError "*"))
            "/", wrap2 "/" (fun a b ->
                    match a,b with 
                    | IntVal x,IntVal y when y<>0 -> IntVal(x/y) | _-> raise (RuntimeError "/"))
            "=", wrap2 "=" (fun a b ->
                    match a,b with
                    | IntVal x,IntVal y   -> BoolVal(x=y)
                    | BoolVal x,BoolVal y -> BoolVal(x=y)
                    | _ -> raise (RuntimeError "="))
            "<", wrap2 "<" (fun a b ->
                    match a,b with IntVal x,IntVal y -> BoolVal(x<y) | _-> raise (RuntimeError "<"))

            // ввод-вывод
            "print", wrap1 "print" (fun v -> printf "%A\n" v; v)
            "read-file", wrap1 "read-file" (function
                | StringVal path -> StringVal(File.ReadAllText path)
                | _ -> raise (RuntimeError "read-file expects a string path"))
            "write-file", wrap2 "write-file" (fun a b ->
                match a,b with
                | StringVal path, StringVal content ->
                    File.WriteAllText(path, content)
                    StringVal path
                | _ -> raise (RuntimeError "write-file expects (string path) (string content)"))

            // базовые списочные операции
            "cons", wrap2 "cons" (fun a b ->
                    match b with
                    | ListVal l -> ListVal (a::l)
                    | _ -> raise (RuntimeError "cons expects a list as second arg"))
            "list", BuiltinFunc(fun args ->
                    ListVal args)
            "head",  wrap1 "head"  (function
                    | ListVal (h::_) -> h
                    | _ -> raise (RuntimeError "head expects non-empty list"))
            "tail",  wrap1 "tail"  (function
                    | ListVal (_::t) -> ListVal t
                    | _ -> raise (RuntimeError "tail expects non-empty list"))
            "null",  wrap1 "null"  (function
                    | ListVal l     -> BoolVal (List.isEmpty l)
                    | _ -> raise (RuntimeError "null expects a list"))
            "length", wrap1 "length" (function
                    | ListVal l -> IntVal (List.length l)
                    | _ -> raise (RuntimeError "length expects a list"))
            "append", wrap2 "append" (fun a b ->
                    match a,b with
                    | ListVal l1, ListVal l2 -> ListVal (l1 @ l2)
                    | _ -> raise (RuntimeError "append expects two lists"))
        ]

    let lookup name =
        match (!env).TryFind name with
        | Some v -> force v
        | None   -> raise (RuntimeError $"Unbound variable {name}")

    match expr with
    | IntLit n      -> IntVal n
    | BoolLit b     -> BoolVal b
    | StringLit s   -> StringVal s

    | Var x ->
        match builtins.TryFind x with
        | Some bf -> bf
        | None    -> lookup x

    | Lambda(p,b)   -> Closure(p,b,ref !env)

    | Apply(f,a) ->
        match force (eval f env) with
        | Closure(p, body, cenv) ->
            let thunk  = Thunk(a, env)
            let newEnv = ref ((!cenv).Add(p, thunk))
            force (eval body newEnv)
        | BuiltinFunc fn ->
            let av = force (eval a env)
            fn [av]
        | _ -> raise (RuntimeError "Attempt to call non-function")

    | If(c,t,e) ->
        match force (eval c env) with
        | BoolVal true  -> eval t env
        | BoolVal false -> eval e env
        | _ -> raise (RuntimeError "Non-boolean guard")

    | Let(n,e1,e2)  ->
        let thunk  = Thunk(e1, env)
        let newEnv = ref ((!env).Add(n, thunk))
        eval e2 newEnv

    | LetRec(n,fdef,body) ->
        let rec recEnv = ref Map.empty
        let thunk      = Thunk(fdef, recEnv)
        recEnv := (!env).Add(n, thunk)
        let newEnv = ref ((!env).Add(n, thunk))
        eval body newEnv

    | ListNil       -> ListVal []
    | ListCons(h,t) ->
        let hv = force (eval h env)
        match force (eval t env) with
        | ListVal l -> ListVal (hv::l)
        | _ -> raise (RuntimeError "Tail is not a list")

    | Builtin name ->
        match builtins.TryFind name with
        | Some (BuiltinFunc fn) -> fn []
        | _ -> raise (RuntimeError $"Unknown builtin {name}")
