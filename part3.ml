(* Honor code comes here:

   First Name: Liang
   Last Name: Han
   BU ID: U86104920

   I pledge that this program represents my own program code and that I have
   coded on my own. I received help from no one in designing and debugging my
   program. I have read the course syllabus of CS 320 and have read the sections
   on Collaboration and Academic Misconduct. I also understand that I may be
   asked to meet the instructor or the TF for a follow up interview on Zoom. I
   may be asked to explain my solution in person and may also ask you to solve a
   related problem. *)

(*NOTE: There are no restrictions on what you can use*)

let and_ex1 = "Push 1\nPush 0\nAnd\nQuit"
let and_ex2 = "Push 1\nPush 1\nAnd\nQuit"
let and_ex3 = "Push 1\nAnd\nQuit"
let and_ex4 = "Push 3\nPush 1\nAnd\nQuit"
let or_ex1 = "Push 1\nPush 0\nOr\nQuit"
let or_ex2 = "Push 0\nPush 0\nOr\nQuit"
let or_ex3 = "Push 1\nOr\nQuit"
let or_ex4 = "Push 3\nPush 1\nOr\nQuit"
let not_ex1 = "Push 1\nPush 0\nNot\nQuit"
let not_ex2 = "Push 1\nPush 1\nNot\nQuit"
let not_ex3 = "Push 3\nNot\nQuit"
let eq_ex1 = "Push 1\nPush 1\nEqual\nQuit"
let eq_ex2 = "Push 1\nPush 1\nPush 1\nAdd\nEqual\nQuit"
let eq_ex3 = "Push 0\nPush 1\nPush 1\nSub\nEqual\nQuit"
let eq_ex4 = "Push \"abc\"\nPush 1\nEqual\nQuit"
let eq_ex5 = "Push 1\nEqual\nQuit"
let lte_ex1 = "Push 1\nPush 1\nLte\nQuit"
let lte_ex2 = "Push 1\nPush 2\nLte\nQuit"
let lte_ex3 = "Push 2\nPush 1\nLte\nQuit"
let lte_ex4 = "Push \"abc\"\nPush 1\nLte\nQuit"
let lte_ex5 = "Push 1\nLte\nQuit"
let local_ex1 = "Push 3\nLocal x\nPush x\nLocal y\nPush x\nPush y\nQuit"
let local_ex2 = "Push 3\nLocal x\nQuit"
let local_ex3 = "Push 2\nLocal x\nPush x\nPush 3\nLocal x\nPush x\nAdd\nPush x\nQuit"
let local_ex4 = "Local x\nQuit"
let local_ex5 = "Push x\nQuit"
let global_ex1 = "Push 3\nGlobal x\nPush x\nQuit"
let global_ex2 = "Push 3\nGlobal x\nQuit"
let global_ex3 = "Push 2\nGlobal x\nPush x\nPush 3\nGlobal x\nPush x\nAdd\nPush x\nQuit"
let global_ex4 = "Push 2\nLocal x\nPush x\nPush 1\nAdd\nGlobal x\nPush x\nQuit"
let global_ex5 = "Global x\nQuit"
let global_ex6 = "Push x\nQuit"

let be_ex1 = "Push 1\nPush 2\nBegin\nPush 3\nPush 7\nPush 4\nEnd\nPush 5\nPush 6\nQuit"
let be_ex2 = "Push 3\nBegin\nPop\nPush 7\nEnd\nQuit"
let be_ex3 = "Push 55\nLocal x\nPush x\nBegin\nPush 3\nPush 5\nLocal x\nPush 7\nPush x\nEnd\nPush x\nQuit"
let be_ex4 = "Push 55\nLocal x\nPush x\nBegin\nPush 3\nPush 5\nGlobal x\nPush 7\nPush x\nEnd\nPush x\nQuit"
let be_ex5 = "Push 55\nGlobal x\nPush x\nBegin\nPush 3\nPush 5\nGlobal x\nPush 7\nPush x\nEnd\nPush x\nQuit"
let be_ex6 = "Begin\nEnd\nQuit"
let be_ex7 = "Push 1\nLocal x\nPush 2\nLocal y\nBegin\nPush 20\nLocal x\nPush x\nPush y\nAdd\nEnd\nPush x\nAdd\nQuit"
let test = [["Push"; "999"]; ["Begin"]; ["Begin"]; ["Push"; "2"]; ["End"]; ["End"]; ["Quit"]]

let myglobal_ex1 = "Push 2\nGlobal test_of_int\nPush test_of_int\nQuit"

let mybe_ex1 = "Push 2\nBegin\nPush 999\nPush 3\nEnd\nPush 4\nBegin\nPush 999\nPush 5\nEnd\nPush 6\nQuit"
let mybe_ex2 = "Push 999\nBegin\nBegin\nEnd\nEnd\nQuit"
let mybe_ex3 = "Push 999\nBegin\nBegin\nPush 5\nEnd\nEnd\nQuit"

let if_ex1 = "Push 10\nPush 1\nIfThen\nPush 5\nAdd\nElse\nPush 5\nSub\nEnd\nQuit"
let if_ex2 = "Push 10\nPush 0\nIfThen\nPush 5\nAdd\nElse\nPush 5\nSub\nEnd\nQuit"
let if_ex3 = "Push 10\nLocal x\nPush 0\nIfThen\nPush 5\nAdd\nElse\nPush x\nPush 234\nLocal x\nPush x\nEnd\nPush x\nQuit"
let myif_ex1 = "Push 1\nPush 1\nIfThen\nPush 5\nIfThen\nPush 100\nElse\nPush 0\nEnd\nElse\nPush 10\nEnd"

let mixed_ex1 = "Push 999\nBegin\nPush 1\nIfThen\nPush 100\nElse\nPush 0\nEnd\nEnd\nQuit"
let mixed_ex2 = "Push 123\nBegin\nPush 0\nIfThen\nBegin\nPush 999\nEnd\nElse\nPush -1\nEnd\nEnd\nQuit"
let mixed_ex3 = "Push 999\nBegin\nPush 0\nIfThen\nBegin\nPush 999\nEnd\nElse\nBegin\nEnd\nPush -1\nEnd\nEnd\nQuit"
let fail = "Push 100\nPush 500\nDiv\nPush \"Python\"\nBegin\nPush 20\nLocal x\nPush \"Pascal\"\nLocal y\nPush \"Java\"\nLocal x\nPush 90\nLocal y\nPush \"HTML\"\nEnd\nPush x\nQuit"

let fail2 = "Push 1\nGlobal one\nBegin\nPush 2\nLocal two\nBegin\nPush one\nPush two\nSub\nEnd\nEnd\nIfThen\nPush \"True\"\nElse\nPush \"False\"\nEnd\nQuit"

let all_ex1 = "Push 5\nLocal x\nPush 0\nGlobal y\nPush x\nBegin\nPush y\nIfThen\nPush \"Positive\"\nElse\nPush \"Negative\"\nEnd\nEnd\nQuit"
let fun_ex1 = "Fun f1 x\nPush x\nReturn\nMut f2 x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush f1\nCall\nReturn\nMut f3 x\nPush x\nPush 1\nAdd\nLocal x\nPush x\nPush f2\nCall\nReturn\nEnd\nPush 3\nPush f3\nCall\nQuit"
let fun_ex2 = "Fun f1 x\nPush y\nReturn\nMut f2 x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush f1\nCall\nReturn\nMut f3 y\nPush y\nPush 1\nAdd\nLocal x\nPush x\nPush f2\nCall\nReturn\nEnd\nPush 3\nPush f3\nCall\nQuit"
let fun_ex3 = "Fun regular x\nPush 11\nPush x\nTuple 2\nReturn\nEnd\nPush 22\nPush regular\nCall\nQuit"
let fun_ex4 = "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nPush even\nCall\nReturn\nEnd\nMut even x\nPush 1\nPush x\nAdd\nLocal x\nPush x\nPush odd\nCall\nReturn\nEnd\nPush 5\nPush odd\nCall\nQuit"
let fun_ex5 = "Fun numOfStepsToOne x\nPush numOfSteps\nPush 1\nAdd\nGlobal numOfSteps\nPush x\nPush 1\nAdd\nLocal x\nPush x\nPush divideByTwo\nCall\nReturn\nMut divideByTwo x\nPush numOfSteps\nPush 1\nAdd\nGlobal numOfSteps\nPush 2\nPush x\nDiv\nLocal x\nPush x\nPush 1\nEqual\nIfThen\nPush numOfSteps\nReturn\nElse\nPush x\nPush numOfStepsToOne\nCall\nReturn\nEnd\nEnd\nPush 0\nGlobal numOfSteps\nPush 5\nPush numOfStepsToOne\nCall\nQuit"
let fun_ex6 = "Fun snack laddoo\nPush laddoo\nPush 100\nMul\nLocal cal\nPush cal\nPush calories\nCall\nReturn\nMut calories cal\nPush 9\nPush cal\nDiv\nLocal g\nPush g\nPush sugar\nCall\nReturn\nMut sugar g\nPush g\nReturn\nEnd\nPush 9\nPush snack\nCall\nQuit"

let test_fun = "Fun f1 x\nPush 1\nReturn\nMut f2 x\nPush 2\nReturn\nMut f3 x\nPush 3\nReturn\nEnd\nQuit";;
let test_fun2 = "Fun f1 x\nPush 1\nReturn\nEnd\nQuit"

let mc_test n = 
"Fun mc n\nPush 100\nPush n\nLte\nIfThen\nPush n\nPush 11\nAdd\nPush mc\nCall\nPush mc\nCall\nReturn\nElse\nPush 10\nPush n\nSub\nReturn\nEnd\nEnd\nPush " ^ (string_of_int n) ^ "\nPush mc\nCall\nQuit";;
let magic =  "Fun f x\nPush x\nReturn\nMut f x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush f\nCall\nReturn\nMut g x\nPush x\nPush 1\nAdd\nLocal x\nPush x\nPush f\nCall\nReturn\nEnd\nPush 3\nPush g\nCall\nQuit"


let read_whole_file filename =
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let replace str = 
  String.fold_right (fun elm acc -> if elm = '\r' then acc else (Char.escaped elm) ^ acc) "" str
let fail1 = "Push \"MutualFuns\"\nFun foo x\nPush x\nPush 0\nEqual\nIfThen\nPush \"a\"\nReturn\nElse\nPush 1\nPush x\nSub\nPush bar\nCall\nPush \"a\"\nConcat\nReturn\nEnd\nMut bar y\nPush y\nPush 0\nEqual\nIfThen\nPush \"b\"\nReturn\nElse\nPush 1\nPush y\nSub\nPush foo\nCall\nPush \"b\"\nConcat\nReturn\nEnd\nEnd\nPush 10\nPush foo\nCall\nQuit\n"

let and_then (f : 'a -> ('b, 'e) result) (res : ('a, 'e) result): ('b, 'e) result =
  Result.bind res f



let all_ok (ls : ('a, 'e) result list): ('a list, 'e) result =
  let combine_result a b =
    a |> and_then @@ fun a ->
    b |> and_then @@ fun b ->
    Ok(b :: a)
  in
  List.fold_left combine_result (Ok([])) ls |> Result.map List.rev

let fst (a,b) = a
let snd (a,b) = b
(*PROGRAM DEFINITIONS*)

type var = Var of string

type const =
  | Int of int
  | String of string
  | Name of var

type com =
  | Quit
  | Push of const
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Swap
  | Neg
  | Concat
  | And
  | Or
  | Not
  | Equal
  | Lte
  | Local of var
  | Global of var
  | BeginEnd of prog
  | IfThenElse of prog * prog
  | InjL
  | InjR
  | CaseLeftRight of prog * prog
  | Tuple of const
  | Get of const
  | Call
  | Return
  | Fun of (var * var * prog) list

and prog = com list

and value =
  | VInt of int
  | VString of string
  | Left of value
  | Right of value
  | Clo of (var * var * prog) * (var * var * prog) list * env
  | Tup of value list

and env = (var * value) list
type stack = value list
type log = string list


type state = stack * log * (* local *) env * (* global *) env


let returning : (string, bool)Hashtbl.t = Hashtbl.create 10
let int_of_var (v : var) : int =
  match v with
  | Var(i) -> int_of_string i

let string_of_var (v : var) : string =
  match v with
  | Var(i) -> i

let int_of_bool (b : bool): int =
  if b then 1 else 0

let is_int (s : string): bool =
  match int_of_string_opt s with
  | Some(i) -> true
  | None -> false
let is_bool (n : int): bool =
  n = 0 || n = 1

let lookup_bind (x : var) (envs : env * env): value option =
  let rec lookup e =
    match e with
    | [] -> None
    | (y, v)::rst -> if y = x
                     then Some(v)
                     else lookup rst
  in
  let (local, global) = envs in
  match lookup local with
  | None -> lookup global
  | Some(v) -> Some(v)

let add_bind (x : var) (v : value) (e : env): env  =
  let updated, e =
      List.fold_left
        (fun (updated, acc) (y, v') ->
          if y = x 
          then 
            (* match v' with
          | Clo(a,b,c) -> false, (y, v')::acc
          | _ ->  *)
            true, (y, v)::acc
          else updated, (y, v')::acc)
        (false, [])
        e
  in
  let e = List.rev e in
  if updated then e else (x, v)::e

(* let add_clo (from : env) (to_ : env): env =
  List.fold_left(fun acc (var,value) ->
    match value with
    | Clo(_,_,_) -> add_bind var value to_
    | _ -> acc
    ) to_ from
   *)
let com_arity (c : com): int =
  match c with
  | Quit | Push(_) | BeginEnd(_) | IfThenElse(_) | Return -> 0
  | Pop  | Neg | Not | Local(_) | Global(_) | InjL | InjR | CaseLeftRight(_) -> 1
  | Call | Add  | Sub | Mul | Div | Swap | Concat | And | Or | Equal | Lte -> 2
  | Fun(_) -> 2
  | Tuple(n) | Get(n) -> 
    match n with
    | Int(v) -> v
    | _ -> failwith "Invalid arity"

let remove target lst =
  List.filter (fun elm -> not (elm = target)) lst
(*PRINTERS*)
let rec string_of_list (p : 'a -> string) (ls : 'a list): string =
  match ls with
  | []       -> "[]"
  | fst::rst -> p fst ^ "::" ^ string_of_list p rst
let rec string_of_tuple (p : 'a -> string) (ls : 'a list): string =
  match ls with
  | []       -> ""
  | fst::[] -> p fst
  | fst::rst -> p fst ^ ", " ^ string_of_tuple p rst

let string_of_const c =
  match c with
  | Int(i)       -> string_of_int i
  | String(s)    -> "\"" ^ s ^ "\""
  | Name(Var(v)) -> v


let rec string_of_value v =
  match v with
  | VInt(i)    -> string_of_int i
  | VString(s) -> "\"" ^ s ^ "\""
  | Left(k) -> "Left " ^ (string_of_value k)
  | Right(k) -> "Right " ^ (string_of_value k)
  | Clo((fname, arg, prog), mutlst, env) -> "Clo (" ^ string_of_var(fname) ^ ", " ^ string_of_var(arg) ^ ")" 
  | Tup(lst) -> "(" ^ string_of_tuple string_of_value lst ^ ")"
 
let rec string_of_com (c : com) : string =
  match c with
  | Quit    -> "Quit"
  | Push(c) -> Printf.sprintf "Push %s" (string_of_const c)
  | Pop     -> "Pop"
  | Add     -> "Add"
  | Sub     -> "Sub"
  | Mul     -> "Mul"
  | Div     -> "Div"
  | Swap    -> "Swap"
  | Neg     -> "Neg"
  | Concat  -> "Concat"
  | And     -> "And"
  | Or      -> "Or"
  | Not     -> "Not"
  | Equal   -> "Equal"
  | Lte     -> "Lte"
  | Local (Var(v))   -> Printf.sprintf "Local %s" v
  | Global(Var(v))   -> Printf.sprintf "Local %s" v
  | BeginEnd(p)      ->
     "Begin\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev p)
   ^ "\nEnd\n"
  | IfThenElse(t, e) ->
     "IfThen\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev t)
   ^ "\nElse\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev e)
   ^ "\nEnd\n"
  | InjL -> "InjL"
  | InjR -> "InjR"
  | CaseLeftRight(l,r) ->
     "CaseLeft\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev l)
   ^ "\nRight\n"
   ^ List.fold_left (fun acc x -> acc ^ "\n" ^ string_of_com x ) "" (List.rev r)
   ^ "\nEnd\n "
  | Call -> "Call"
  | Return -> "Return"
  | Tuple(i) -> "Tuple " ^ string_of_const i 
  | Get(i) -> "Get" ^ string_of_const i
  | Fun(lst) -> "Fun Clo" 

let clo_exists clo fname = 
  match clo with
  | Clo((fun_name,_,_),_,_) when fun_name = fname -> true
  | _ -> false 
let add_clo env lst = 
  let make_clo elm lst' =
  let rst = remove elm lst' in
  match elm with
    | (fname, arg, prog) -> Clo(elm,rst,env)
in
  let clo_lists lst = List.rev (List.fold_left (fun acc elm ->
    make_clo elm lst :: acc
    ) [] lst)
  in
  List.fold_left (fun acc (Clo((fname,arg,prog),rst,env1))  ->
    add_bind fname (Clo((fname,arg,prog),rst,env1)) acc
    ) env (clo_lists lst)

(*TOKENIZING*)
let char_list_of_string (s : string): char list =
  List.init (String.length s) (String.get s)

type token =
  | NEWLINE
  | STRING of string
  | NUMBER of int
  | SYMBOL of string

let string_of_token tok =
  match tok with
  | NEWLINE   -> "\\n"
  | STRING(s) -> "\"" ^ s ^ "\""
  | NUMBER(n) -> string_of_int n
  | SYMBOL(s) -> s

let print_head ls =
  print_endline("print_head: " ^ string_of_token (List.hd ls))

let print_head_env env =
  match env with
  | (name,value) :: rst -> print_endline("(" ^ string_of_var name ^ " -> " ^ string_of_value value ^ ")")
  | [] -> print_endline("Empty env!")
let print_elm s = 
  print_string (string_of_token s ^ " ")
let print_stack (stack,log,local,global: state) : unit =
  let rec string_of_stack (p : 'a -> string) (ls : 'a list): string =
  match ls with
  | []       -> ""
  | fst::[] -> p fst
  | fst::rst -> p fst ^ ", " ^ string_of_stack p rst
  in
  print_endline("[" ^ string_of_stack string_of_value stack ^ "]")

let int_of_const c =
  match c with
  | Int(i) -> i
  | _ -> -1
let is_space (c : char): bool =
  c = ' ' || c = '\t'

let is_digit (c : char): bool =
  match c with | '0' .. '9' -> true | _ -> false

let is_alpha (c : char): bool =
  match c with | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let int_of_char (c : char): int =
  int_of_string @@ Char.escaped c

type lexer_err =
  | UnclosedString of (* line number *) int
  | InvalidChar    of (* line number *) int * char
  | UnknownChar    of (* line number *) int * char

let string_of_lexer_err e =
  match e with
  | UnclosedString(i) -> Printf.sprintf "Unclosed string at line %i" i
  | InvalidChar(i, c) -> Printf.sprintf "Invalid char '%c' at line %i" c i
  | UnknownChar(i, c) -> Printf.sprintf "Unknown char '%c' at line %i" c i


let tokenize_string line_num (ls : char list): (string * char list, lexer_err) result =
  let rec helper ls acc =
    match ls with
    | [] -> Error(UnclosedString(line_num))
    | ch::rst ->
       if ch = '\"' then
         Ok(acc, rst)
       else if is_alpha ch then
         helper rst (acc ^ Char.escaped ch)
       else
         Error(InvalidChar(line_num, ch))
  in helper ls ""

let tokenize_number line_num (ls : char list): (int * char list, lexer_err) result =
  let rec helper ls acc =
    match ls with
    | [] -> Ok(acc, [])
    | ch::rst ->
       if ch = '\n' || is_space ch then
         Ok(acc, ls)
       else if is_digit ch then
         helper rst @@ acc * 10 + int_of_char ch
       else Error(InvalidChar(line_num, ch))
  in helper ls 0

let tokenize_symbol line_num (ls : char list): (string * char list, lexer_err) result =
  let rec helper ls acc =
    match ls with
    | [] -> Ok(acc, [])
    | ch::rst ->
       if ch = '\n' || is_space ch then
         Ok(acc, ls)
       else if is_alpha ch || ch = '_' || is_digit ch then
         helper rst (acc ^ Char.escaped ch)
       else
         Error(InvalidChar(line_num, ch))
  in helper ls ""

let tokenize_source (src : string): (token list, lexer_err) result =
  let rec helper line_num ls acc =
    match ls with
    | [] -> Ok(List.rev @@ acc)
    | ch::rst ->
       match ch with
       | '\n' -> helper (line_num + 1) rst (NEWLINE :: acc)
       | '\"' -> tokenize_string line_num rst |> and_then @@ fun (s, rst) ->
                 helper line_num rst (STRING(s) :: acc)
       | '-'  -> tokenize_number line_num rst |> and_then @@ fun (n, rst) ->
                 helper line_num rst (NUMBER(-1 * n) :: acc)
       | ch when is_digit ch
              -> tokenize_number line_num (ch::rst) |> and_then @@ fun (n, rst) ->
                 helper line_num rst (NUMBER(n) :: acc)
       | ch when is_alpha ch
              -> tokenize_symbol line_num ls |> and_then @@ fun (s, rst) ->
                 helper line_num rst (SYMBOL(s) :: acc)
       | ch when is_space ch -> helper line_num rst acc
       | ch -> Error(UnknownChar(line_num, ch))
  in helper 1 (char_list_of_string src) []


(*PARSING*)
type parse_err =
  | MissingArguments of (* line number *) int
  | InvalidCom       of (* line number *) int * token
  | ExpectedConst    of (* line number *) int * token
  | ExpectedVar      of (* line number *) int * token
  | InvalidVar       of (* line number *) int * token
  | UnexpectedEOF    of (* line number *) int
  | MissingNewline   of (* line number *) int
  | MissingFun       of (* line number *) int
  | EarlyReturn      of (* line number *) int
  | SoloMut          of                   int

let string_of_parse_err e =
  match e with
  | MissingArguments(i) ->
     Printf.sprintf "Missing arguments to command at line %i" i
  | InvalidCom(i, tok) ->
     Printf.sprintf "Invalid command at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedConst(i, tok) ->
     Printf.sprintf "Expected constant at line %i, got: \"%s\"" i (string_of_token tok)
  | ExpectedVar(i, tok) ->
     Printf.sprintf "Expected a variable name at line %i, got: \"%s\"" i (string_of_token tok)
  | InvalidVar(i, tok) ->
     Printf.sprintf "Invalid variable name at line %i, got: \"%s\"" i (string_of_token tok)
  | UnexpectedEOF(i) ->
     Printf.sprintf "Ran out of tokens at line %i" i
  | MissingNewline(i) ->
     Printf.sprintf "Missing newline on line %i" i
  | MissingFun(i) ->
    Printf.sprintf "Missing Fun on line %i (Mut without Fun)" i
  | EarlyReturn(i) ->
    Printf.sprintf "false too early on line %i (more commands after return)" i
  | SoloMut(i) ->
    Printf.sprintf "Mut without Fun at line %s" (string_of_int i)
    

(* DEPENDS ON: Symbol tokens being valid variables with arbitrary case starting char *)
let make_var line_num (s : string): (var, parse_err) result =
  if String.length s <> 0 then
     match String.get s 0 with
     | 'a' .. 'z' -> Ok(Var(s))
     | _ -> Error(InvalidVar(line_num, SYMBOL(s)))
  else Error(InvalidVar(line_num, SYMBOL(s)))

(* Consume a newline from the token list, if it is required and not present, error. *)
let consume_newline (line_num : int) (required : bool) (ls : token list)
                  : (int * token list, parse_err) result =
  (* print_endline("consume: " ^ string_of_token (List.hd ls)); *)
  match ls with
  | [] -> Ok(line_num, [])
  | NEWLINE::tl -> Ok((line_num + 1, tl))
  |      hd::tl -> if required then
                     Error(MissingNewline(line_num))
                   else
                     Ok(line_num, hd::tl)

(* See: PA4 parse_sexpr *)
let rec parse_com line_num ls : (com * int * token list, parse_err) result =
  (* let command (SYMBOL(s)) = s in
  print_endline("command is: " ^ command (List.hd ls)); *)
  let parse_const line_num tok =
    match tok with
    | NUMBER(n) -> Ok(Int(n))
    | STRING(s) -> Ok(String(s))
    | SYMBOL(s) -> make_var line_num s |> Result.map (fun x -> Name(x))
    | tok       -> Error(ExpectedConst(line_num, tok))
  in
    match ls with
  | SYMBOL("Push")  ::fst::rst ->
     parse_const line_num fst |> and_then @@ fun x ->
     Ok(Push(x), line_num, rst)
  | SYMBOL("Quit")  ::rst -> Ok(Quit, line_num, rst)
  | SYMBOL("Pop")   ::rst -> Ok(Pop, line_num, rst)
  | SYMBOL("Add")   ::rst -> Ok(Add, line_num, rst)
  | SYMBOL("Sub")   ::rst -> Ok(Sub, line_num, rst)
  | SYMBOL("Mul")   ::rst -> Ok(Mul, line_num, rst)
  | SYMBOL("Div")   ::rst -> Ok(Div, line_num, rst)
  | SYMBOL("Swap")  ::rst -> Ok(Swap, line_num, rst)
  | SYMBOL("Neg")   ::rst -> Ok(Neg, line_num, rst)
  | SYMBOL("Concat")::rst -> Ok(Concat, line_num, rst)
  | SYMBOL("And")   ::rst -> Ok(And, line_num, rst)
  | SYMBOL("Or")    ::rst -> Ok(Or, line_num, rst)
  | SYMBOL("Not")   ::rst -> Ok(Not, line_num, rst)
  | SYMBOL("Equal") ::rst -> Ok(Equal, line_num, rst)
  | SYMBOL("Lte")   ::rst -> Ok(Lte, line_num, rst)

  | SYMBOL("Local") ::SYMBOL(s)::rst ->
     make_var line_num s |> Result.map @@ fun x -> Local(x), line_num, rst
  | SYMBOL("Local") ::c::rst -> Error(ExpectedVar(line_num, c))

  | SYMBOL("Global")::SYMBOL(s)::rst ->
     make_var line_num s |> Result.map @@ fun x -> Global(x), line_num, rst
  | SYMBOL("Global")::c::rst -> Error(ExpectedVar(line_num, c))

  | SYMBOL("Begin") ::rst ->
     consume_newline line_num false rst
     |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("End")) rst
     |> and_then @@ fun (blk, line_num, rst) ->
     Ok(BeginEnd(blk), line_num, rst)

  | SYMBOL("IfThen")::rst ->
     consume_newline line_num false rst
       |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("Else")) rst
       |> and_then @@ fun (then_blk, line_num, rst) ->
     consume_newline line_num false rst
       |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("End")) rst
       |> and_then @@ fun (else_blk, line_num, rst) ->
     Ok(IfThenElse(then_blk, else_blk), line_num, rst)
  | SYMBOL("InjL") :: rst -> Ok(InjL, line_num, rst)
  | SYMBOL("InjR") :: rst -> Ok(InjR, line_num, rst)
  | SYMBOL("CaseLeft")::rst ->
     consume_newline line_num false rst
       |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("Right")) rst
       |> and_then @@ fun (left_blk, line_num, rst) ->
     consume_newline line_num false (print_head rst;rst)
       |> and_then @@ fun (line_num, rst) ->
     parse_com_list line_num (SYMBOL("End")) rst
       |> and_then @@ fun (right_blk, line_num, rst) ->
     Ok(CaseLeftRight(left_blk, right_blk), line_num, rst)
  | SYMBOL("Tuple"):: fst :: rst -> 
    parse_const line_num fst |> and_then @@ fun x -> Ok(Tuple(x), line_num, rst)
  | SYMBOL("Get")  :: fst :: rst ->
    parse_const line_num fst |> and_then @@ fun x -> Ok(Get(x), line_num, rst)
      
  | SYMBOL("Fun") :: SYMBOL(fst) :: SYMBOL(snd) :: rst -> 
    let rec parse_fun line_num rst acc =
      match rst with
      | SYMBOL(fst) :: SYMBOL(snd) :: rst' -> 
        consume_newline line_num false rst' |> and_then @@ fun (line_num, rst) ->
        make_var line_num fst |> and_then @@ fun fname ->
        make_var line_num snd |> and_then @@ fun arg ->
        parse_fun_list line_num rst |> and_then @@ fun (ending, prog, line_num, rst) ->
      if ending then Ok(Fun((fname, arg, prog)::acc), line_num, rst)
      else
        parse_fun line_num rst ((fname, arg, prog)::acc)
      | [] -> Error(UnexpectedEOF(line_num))
      | _ -> Error(InvalidCom(line_num,List.hd rst))
      in
    parse_fun line_num (SYMBOL(fst) :: SYMBOL(snd) :: rst) []
          
  | SYMBOL("Call") :: rst -> Ok(Call, line_num, rst)
  | SYMBOL("Return")::rst -> Ok(Return, line_num, rst)
     
  | tok::_ -> Error(InvalidCom(line_num, tok))
  | [] -> Error(UnexpectedEOF(line_num))
  
  


(* See: PA4 parse_sexpr_list *)
and parse_com_list (line_num : int) (terminator : token) (ls : token list)
                 : (prog * int * token list, parse_err) result =
 (* print_string("From parse_list: "); print_head ls; *)
    match ls with
    | fst::rst when fst = terminator -> Ok([], line_num, rst)
    | _  -> parse_com line_num ls
              |> and_then @@ fun (fst, line_num, ls') ->
            (* print_string("From parse_list: "); *)
            consume_newline line_num true ls'
              |> and_then @@ fun (line_num, ls'') ->
            parse_com_list line_num terminator ls''
              |> and_then @@ fun (rst, line_num, ls''') ->
            Ok((fst::rst, line_num, ls'''))

and parse_fun_list (line_num : int) (ls : token list)
                 : (bool * prog * int * token list, parse_err) result =
                 (* parse until mut or end *)
 (* print_string("From parse_list: "); print_head ls; *)
    match ls with
    | fst::rst when fst = SYMBOL("Mut") -> Ok(false, [], line_num, (rst))
    | fst::rst when fst = SYMBOL("End") -> Ok(true, [], line_num, rst)
    | _  -> (parse_com line_num ls)
              |> and_then @@ fun (fst, line_num, ls') ->
            (* print_string("From parse_list: "); *)
            consume_newline line_num true ls'
              |> and_then @@ fun (line_num, ls'') ->
            parse_fun_list line_num ls''
              |> and_then @@ fun (ending, rst, line_num, ls''') ->
            Ok((ending, fst::rst, line_num, ls'''))

(* and parse_mut_com (line_num : int) (ls : token list)
                 : (com * int * token list, parse_err) result =
 (* print_string("From parse_list: "); print_head ls; *)
    match ls with
    | SYMBOL("Mut") :: SYMBOL(fst) :: SYMBOL(snd) :: rst ->
    make_var line_num fst |> and_then @@ fun fname ->
    make_var line_num snd |> and_then @@ fun arg ->
    consume_newline line_num false rst |> and_then @@ fun (line_num, rst) ->
    parse_fun_list line_num rst |> and_then @@ fun (ending, prog, line_num, rst) ->
      Ok(Mut(fname,arg,prog), line_num, rst)
    | _ -> parse_com line_num ls *)
(* See: PA4 parse *)
and parse_program (src : token list): (prog, parse_err) result  =
  let rec parse_all line_num ls acc  =
    match ls with
    | [] -> Ok(List.rev acc)
    | _  -> parse_com line_num ls
              |> and_then @@ fun (c, line_num, ls') ->
            (* print_string("From parse_all: "); *)
            consume_newline line_num true ls'
              |>  and_then @@ fun (line_num, ls'') ->
            parse_all line_num ls'' (c::acc)
  in
  parse_all 1 src []

(*EVALUATION*)
let ret_in_fun = Stack.create()

type eval_err =
  | InvalidArity of com * (* # got *) int
  | WrongType    of com * (* args got *) value list
  | DivByZero    of int * int
  | UnboundVar   of var
  | NoValue      of com
  | IndexOutOfBound of const
  | NotATuple    of value
  | SoloMut
  | NoReturn
  | NotAFunction
  | ReturnOutSideOfFunction
  | InvalidTuple of const

let string_of_eval_err e =
  match e with
  | InvalidArity(c, i) ->
     Printf.sprintf "Got %i arguments to %s, expected %i" i (string_of_com c) (com_arity c)
  | WrongType(_, ls) ->
     Printf.sprintf "Got arguments of incorrect type: " ^ string_of_list string_of_value ls
  | DivByZero(m, n) ->
     Printf.sprintf "Got arguments to div: %i / %i" m n
  | UnboundVar(Var(s)) ->
     Printf.sprintf "Unbound variable %s" s
  | NoValue(c) ->
     Printf.sprintf "Expected return value from command %s" (string_of_com c)
  | IndexOutOfBound(i) ->
    Printf.sprintf "Index Error: Index %s out of bound" (string_of_const i)
  | NotATuple(c) ->
    Printf.sprintf "Expected a tuple, but found %s." (string_of_value c)
  | SoloMut -> 
    Printf.sprintf "Cannot use Mut without Fun"
  | NotAFunction ->
    Printf.sprintf "Callee is not a function"
  | NoReturn ->
    Printf.sprintf "Function ends without a return" 
  | ReturnOutSideOfFunction ->
    Printf.sprintf "Returns when not in a function" 
  | InvalidTuple(c) ->
    Printf.sprintf "Tuple is negative or not a int" 

let with_stack (f : stack -> (stack, 'e) result) (s, l, loc, glob : state): (state, 'e) result =
  f s |> Result.map (fun s -> s, l, loc, glob)

(*PROGRAM METHODS*)
let quit (stk, log, loc, glob : state): state =
  stk
  , (List.fold_right
       (fun elem acc -> string_of_value elem :: acc)
       stk
       [])
    @ log
  , loc
  , glob

let push (stk, log, loc, glob : state) (c : const): (state, eval_err) result =
  match c with
  | Name(v)   -> lookup_bind v (loc, glob)
              |> Option.to_result ~none:(UnboundVar(v))
              |> Result.map (fun x -> x::stk, log, loc, glob)
  | String(s) -> Ok(VString(s) :: stk, log, loc, glob)
  | Int(n)    -> Ok(VInt(n) :: stk, log, loc, glob)

let pop : state -> (state, eval_err) result =
  with_stack @@
    function
    | []    -> Error(InvalidArity(Pop, 0))
    | _::tl -> Ok(tl)
let add : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x + y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Add, List.length stk))
    | x :: y :: _               -> Error(WrongType(Add, [x; y]))

let sub : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x - y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Sub, List.length stk))
    | x :: y :: _               -> Error(WrongType(Sub, [x; y]))

let mul : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x * y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Mul, List.length stk))
    | x :: y :: _               -> Error(WrongType(Mul, [x; y]))

let div : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(0) :: _   -> Error(DivByZero(x, 0))
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(x / y) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Div, List.length stk))
    | x :: y :: _               -> Error(WrongType(Div, [x; y]))

let swap : state -> (state, eval_err) result =
  with_stack @@
    function
    | x :: y :: rst       -> Ok(y :: x :: rst)
    | _ :: [] | [] as stk -> Error(InvalidArity(Swap, List.length stk))

let neg : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: rst -> Ok(VInt(-1 * x) :: rst)
    | []             -> Error(InvalidArity(Neg, 0))
    | x :: _         -> Error(WrongType(Neg, [x]))

let concat : state -> (state, eval_err) result =
  with_stack @@
    function
    | VString(x) :: VString(y) :: rst -> Ok(VString(x ^ y) :: rst)
    | _ :: [] | [] as stk             -> Error(InvalidArity(Concat, List.length stk))
    | x :: y :: _                     -> Error(WrongType(Concat, [x; y]))

let and_ : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst when is_bool x && is_bool y
                          -> Ok(VInt(int_of_bool (x = y)) :: rst)
    | _ :: [] | [] as stk -> Error(InvalidArity(And, List.length stk))
    | x :: y :: _         -> Error(WrongType(And, [x; y]))

let or_ : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst when is_bool x && is_bool y
                          -> Ok(VInt(int_of_bool (x = 1 || y = 1)) :: rst)
    | _ :: [] | [] as stk -> Error(InvalidArity(Or, List.length stk))
    | x :: y :: _         -> Error(WrongType(Or, [x; y]))

let not_ : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: rst when is_bool x
             -> Ok(VInt(abs(x - 1)) :: rst)
    | []     -> Error(InvalidArity(Not, 0))
    | x :: _ -> Error(WrongType(Not, [x]))

let equal : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(int_of_bool (x = y)) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Equal, List.length stk))
    | x :: y :: _               -> Error(WrongType(Equal, [x; y]))

let lte : state -> (state, eval_err) result =
  with_stack @@
    function
    | VInt(x) :: VInt(y) :: rst -> Ok(VInt(int_of_bool (x <= y)) :: rst)
    | _ :: [] | [] as stk       -> Error(InvalidArity(Lte, List.length stk))
    | x :: y :: _               -> Error(WrongType(Lte, [x; y]))

let local (s, l, loc, glob : state) (x : var) : (state, eval_err) result =
  match s with
  | v::rst -> Ok((rst, l, add_bind x v loc, glob))
  | []     -> Error(InvalidArity(Local(x), 0))

let global (s, l, loc, glob : state) (x : var) : (state, eval_err) result =
  match s with
  | v::rst -> Ok((rst, l, loc, add_bind x v glob))
  | []     -> Error(InvalidArity(Global(x), 0))

let injl (s, l, loc, glob : state) : (state, eval_err) result =
  match s with
  | v::rst -> Ok((Left(v) :: rst, l, loc, glob))
  | [] -> Error(InvalidArity(InjL,0))

let injr (s, l, loc, glob : state) : (state, eval_err) result =
  match s with
  | v::rst -> Ok((Right(v) :: rst, l, loc, glob))
  | [] -> Error(InvalidArity(InjR,0))

let tuple (s, l, loc, glob : state) (x : const): (state, eval_err) result =
  if int_of_const x < 0 then Error(InvalidTuple(x))
  else
  let rec make_list (st:stack) (count : int): (value list * stack) =
    if count > 0 then
      match st with
      | v::rst -> let next = make_list rst (count-1) in 
                  (v::(fst next),snd next)
      | [] -> ([],st)
    else
      ([],st)
    in
  let calculate = make_list s (int_of_const x) in
  let lst = List.rev (fst calculate) in
  let rst = snd calculate in
  if List.length lst = (int_of_const x) then
    Ok(Tup(lst)::rst, l, loc, glob)
  else
    Error(InvalidArity(Tuple(Int(int_of_const x)),List.length lst))

let get (s, l, loc, glob : state) (c : const): (state, eval_err) result =
  match s with
  | Tup(lst) :: rst ->
    let index = int_of_const c in
    if index < List.length lst then
    let elm = List.nth lst index in
    Ok(elm::Tup(lst)::rst, l, loc, glob)
    else
      Error(IndexOutOfBound(c))
  | _ -> Error(NotATuple(List.hd s))

(* let _ = Hashtbl.replace env_memory "mem" [] *)

let rec begin_end (s, l, loc, glob : state) (blk : prog): (bool * state, eval_err) result =
  eval blk ([], l, loc, glob) |> and_then @@ fun (quitting, (s', l, _, glob)) ->
  match s' with
  | v::rst -> Ok((quitting, (v::s, l, loc, glob)))
  | []     -> Error(NoValue(BeginEnd(blk)))


and ifthen_else (s, l, loc, glob : state) (then_blk : prog) (else_blk : prog): (bool * state, eval_err) result =
  match s with
  | VInt(v)::rst when v = 1
    -> eval then_blk (rst, l, loc, glob)
  | VInt(v)::rst when v = 0
    -> eval else_blk (rst, l, loc, glob)
  | []     -> Error(InvalidArity(IfThenElse(then_blk, else_blk), 0))
  | x::rst -> Error(WrongType(IfThenElse(then_blk, else_blk), [x]))

and caseleft_right (s, l, loc, glob : state) (left_blk : prog) (right_blk : prog) : (bool * state, eval_err) result =
  match s with
  | Left(v)::rst -> eval left_blk (v::rst, l, loc, glob)
  | Right(v)::rst -> eval right_blk (v::rst, l, loc, glob)
  | []     -> Error(InvalidArity(CaseLeftRight(left_blk, right_blk), 0))
  | x::rst -> Error(WrongType(CaseLeftRight(left_blk, right_blk), [x]))

and fun_ (s, l, loc, glob : state) (lst : (var * var * prog) list): (state, eval_err) result =
   Ok(s, l, add_clo loc lst, glob)

and call (s, l, loc, glob : state) : (bool * state, eval_err) result =
  match s with
  | Clo((fname,arg,prog),muts,env) :: varg :: rst ->
    let full_list = (fname,arg,prog) :: muts in
    let new_env = add_clo env full_list in
    let new__env = add_bind arg varg new_env in
    eval prog ([], l, new__env, glob) |> and_then @@ fun (quitting, (s', l, _, glob) )->
      let returns = Hashtbl.find returning "ret" in
      if not returns then Error(NoReturn) else
      (Hashtbl.replace returning "ret" false; 
      (* print_stack (s,l,loc,glob); *)
      Ok(quitting, (List.hd s'::rst, l, loc, glob)))
  | fst :: [] -> Error(InvalidArity(Call,1))
  | [] -> Error(InvalidArity(Call,0))
  | _ -> Error(NotAFunction)


  

and return (s, l, loc, glob : state) : (bool * state, eval_err) result =
  if Stack.is_empty ret_in_fun then Error(ReturnOutSideOfFunction) else(
 Stack.pop ret_in_fun;
 Hashtbl.replace returning "ret" true;
  match s with
  | v::rst -> Ok(false,(v::[], l, loc, glob))
  | [] -> Error(NoValue(Return))
  )

(* and eval_fun (prog : prog) (st : state) : (bool * state, eval_err) result =
    match prog with
    | Return :: rst -> return st
    |  _ -> eval prog st |> fun (quitting, st) -> Ok(false, quitting, st) *)

and eval (prog : prog) (st : state) : (bool * state, eval_err) result =
  match prog with
  | []                -> Ok(false, st)
  | Quit      :: _    -> print_stack st;Hashtbl.replace returning "ret" true;Hashtbl.replace returning "ret" true;Ok(true, quit st);
  | Push(c)   :: prog -> push   st c |> and_then (eval prog)
  | Pop       :: prog -> pop    st   |> and_then (eval prog)
  | Add       :: prog -> add    st   |> and_then (eval prog)
  | Sub       :: prog -> sub    st   |> and_then (eval prog)
  | Mul       :: prog -> mul    st   |> and_then (eval prog)
  | Div       :: prog -> div    st   |> and_then (eval prog)
  | Swap      :: prog -> swap   st   |> and_then (eval prog)
  | Neg       :: prog -> neg    st   |> and_then (eval prog)
  | And       :: prog -> and_   st   |> and_then (eval prog)
  | Or        :: prog -> or_    st   |> and_then (eval prog)
  | Not       :: prog -> not_   st   |> and_then (eval prog)
  | Lte       :: prog -> lte    st   |> and_then (eval prog)
  | Concat    :: prog -> concat st   |> and_then (eval prog)
  | Equal     :: prog -> equal  st   |> and_then (eval prog)
  | Local(x)  :: prog -> local  st x |> and_then (eval prog)
  | Global(x) :: prog -> global st x |> and_then (eval prog)

  | BeginEnd(p)      :: prog -> begin_end st p
                                |> and_then @@ fun (quitting, st) ->
                                if not quitting then eval prog st else Ok(quitting, st)
  | IfThenElse(t, e) :: prog -> ifthen_else st t e
                                |> and_then @@ fun (quitting, st) ->
                                if not quitting then eval prog st else Ok(quitting, st)
  | InjL      :: prog -> injl st     |> and_then (eval prog)
  | InjR      :: prog -> injr st     |> and_then (eval prog)
  | CaseLeftRight(l, r)::prog -> caseleft_right st l r 
                                |> and_then @@ fun (quitting, st) ->
                                if not quitting then eval prog st else Ok(quitting, st)
  | Tuple(c) :: prog -> tuple st c |> and_then (eval prog)
  | Get(c)   :: prog -> get st c |> and_then (eval prog)
  | Fun(lst) :: prog -> fun_ st lst |> and_then (eval prog)
  | Call     :: prog -> Stack.push "true" ret_in_fun ; Hashtbl.replace returning "ret" false; call st |> and_then @@ fun (quitting, st) ->
                                if not quitting then eval prog st else Ok(quitting, st)
  | Return   :: prog -> return st

let write_file_with_log (file_path: string) (log: log) : unit =
  let fp = open_out file_path in
    let _ =
      List.fold_left (
        fun items_left elem ->
          match items_left with
          | 1 -> Printf.fprintf fp "%s" elem; items_left - 1
          | _ -> Printf.fprintf fp "%s\n" elem; items_left - 1
      ) (List.length log) log
    in close_out fp

type interp_err =
  | LexerErr of lexer_err
  | ParseErr of parse_err
  | EvalErr  of eval_err

let lexer_err e = LexerErr(e)
let parse_err e = ParseErr(e)
let eval_err  e = EvalErr(e)

let string_of_interp_err e =
  match e with
  | LexerErr(e) -> string_of_lexer_err e
  | ParseErr(e) -> string_of_parse_err e
  | EvalErr(e)  -> string_of_eval_err  e

let interpreter (src : string) (output_file_path: string): unit =
  let run src =
    tokenize_source src        |> Result.map_error lexer_err |> and_then @@ fun tokens ->
    parse_program tokens       |> Result.map_error parse_err |> and_then @@ fun prog ->
    eval prog ([], [], [], []) |> Result.map_error eval_err
  in
  Hashtbl.replace returning "ret" false; Stack.clear ret_in_fun;
  (match run src with
   | Ok(_,(_, log, _, _)) -> log
   | Error(e)      -> print_endline("[\"Error\"]");print_endline (string_of_interp_err e); ["\"Error\""])
  |> write_file_with_log output_file_path


let o = "output.txt"

let parse src = tokenize_source src        |> Result.map_error lexer_err |> and_then @@ fun tokens ->
    parse_program tokens       |> Result.map_error parse_err 

let run str = interpreter str o

let test =  [(Var "f3", Var "x", [Push (Int 3); Return]);
    (Var "f2", Var "x", [Push (Int 2); Return]);
    (Var "f1", Var "x", [Push (Int 1); Return])]
  
let env = add_bind (Var("x")) (VInt(1)) []