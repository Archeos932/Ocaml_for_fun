type expr =
  | Const of int
  | Bool  of bool
  | Var   of string
  | Add   of expr * expr
  | Mul   of expr * expr
  | Sub   of expr * expr
  | If    of expr * expr * expr
  | Let   of string * expr * expr
  | Fun   of string * expr
  | App   of expr * expr

module type EnvSig = sig
  type 'a t
    val empty : 'a t
    val bind : string -> 'a -> 'a t -> 'a t
    val lookup : string -> 'a t -> 'a
    val lookup_opt : string -> 'a t -> 'a option

end
module Env : EnvSig = struct
  module M = Map.Make(String)
  type 'a t = 'a M.t
  let empty      = M.empty
  let bind k v e = M.add k v e
  let lookup k e = M.find k e
  let lookup_opt k e = M.find_opt k e
end ;;


module StringSet = Set.Make(String)
type value =
  |VInt of int
  |VBool of bool
  | VFun of string * expr * env
and env = value Env.t


exception RuntimeError    of string
exception UnboundVariable of string
exception TypeError       of string


(*
let Programme_A = (Let ("x", Const 3, Add(Mul(Var "x",Var "x"),Const 1)) )
let Programme_B = If (Add(Const 2,Const 3),Cons 1,Const 0)
let Programme_C = Let ("double", Fun ("n", Mul (Var "n", Const 2)),
  App (Var "double", Const 5))
  *)


let rec size = function
  | Const _ | Bool _ | Var _ -> 1
  | Add(a, b) | Mul(a, b) | Sub(a, b) -> 1 + size a + size b
  | If(c, a, b) -> 1 + size a + size b
  | Let(_, def, body) -> 1 + size def + size body
  | Fun(_, body) -> 1 + size body
  | App(f, arg) -> 1 + size f + size arg

let rec depth = function
  | Const _ | Bool _ | Var _ -> 0
  | Add(a, b) | Mul(a, b) | Sub(a, b) -> 1 + max (depth a) (depth b)
  | If(c, a, b) ->  1 + max (depth a)  (max (depth b) (depth c))
  | Let(_, def, body) -> 1 + max (depth def) (depth body)
  | Fun(_, body) -> 1 + depth body
  | App(f, arg) ->  1 + max (depth f) (depth arg)

let rec count_vars = function
  | Const _ | Bool _ -> 0
  | Var _ -> 1
  | Add(a, b) | Mul(a, b) | Sub(a, b) -> depth a + (depth b)
  | If(c, a, b) ->    (depth a) +  (depth b)+ (depth c)
  | Let(_, def, body) -> (depth def) + (depth body)
  | Fun(_, body) -> depth body
  | App(f, arg) ->  (depth f) + (depth arg)

let rec print_tree ?(indent=0) e =
  let pad = String.make indent ' ' in
  match e with
  | Const n  -> Printf.printf "%sConst %d\n" pad n
  | Bool b   -> Printf.printf "%sBool %b\n" pad b
  | Var x    -> Printf.printf "%sVar %s\n" pad x
  | Add(a,b) ->
      Printf.printf "%sAdd\n" pad;
      print_tree ~indent:(indent+2) a;
      print_tree ~indent:(indent+2) b
  | Mul(a,b) ->
      Printf.printf "%sMul\n" pad;
      print_tree ~indent:(indent+2) a;
      print_tree ~indent:(indent+2) b
  | Sub(a,b) ->
      Printf.printf "%sSub\n" pad;
      print_tree ~indent:(indent+2) a;
      print_tree ~indent:(indent+2) b
  | If(c,a,b) ->
      Printf.printf "%sIf\n" pad;
      print_tree ~indent:(indent+2) c;
      print_tree ~indent:(indent+2) a;
      print_tree ~indent:(indent+2) b
  | Let(x, def, body) ->
      Printf.printf "%sLet %s\n" pad x;
      print_tree ~indent:(indent+2) def;
      print_tree ~indent:(indent+2) body
  | Fun(x, body) ->
      Printf.printf "%sFun %s\n" pad x;
      print_tree ~indent:(indent+2) body
  | App(f, arg) ->
      Printf.printf "%sApp\n" pad;
      print_tree ~indent:(indent+2) f;
      print_tree ~indent:(indent+2) arg


let rec free_vars = function
  | Const _ |Bool _ -> StringSet.empty
  | Var x -> StringSet.singleton x
  | Add (a, b) | Mul (a, b)| Sub (a, b) | App (a,b) -> StringSet.union (free_vars a)  (free_vars b)
  | If (a,b,c) -> StringSet.union (free_vars a)  (StringSet.union (free_vars b) (free_vars c))
  | Let (x,def,body) -> StringSet.union (free_vars def) (StringSet.remove x (free_vars body))
  | Fun (x,body) -> StringSet.remove x  (free_vars body)


let is_closed expr = StringSet.empty = free_vars expr


let check_env e env =
  StringSet.for_all
     (fun x -> Env.lookup_opt x env <> None)
     (free_vars e)


let rec eval env = function
  | Const n -> VInt n
  | Bool b  -> VBool b
  | Var x   ->
      (try Env.lookup x env
       with Not_found -> raise (UnboundVariable x))
  | Fun(x, body) ->
      VFun(x, body, env)
  | Add(a, b) ->
       (match eval env a, eval env b with
        | VInt x, VInt y -> VInt (x + y)
        | _ -> raise (TypeError "Add: expected two ints"))
   | Mul(a, b) ->
       (match eval env a, eval env b with
        | VInt x, VInt y -> VInt (x * y)
        | _ -> raise (TypeError "Mul: expected two ints"))
   | Sub(a, b) ->
       (match eval env a, eval env b with
        | VInt x, VInt y -> VInt (x - y)
        | _ -> raise (TypeError "Sub: expected two ints"))
   | If(cond, e1, e2) ->
       (match eval env cond with
        | VBool true  -> eval env e1
        | VBool false -> eval env e2
        | _ -> raise (TypeError "If: condition must be bool"))
   | Let(x, def, body) ->
        let v = eval env def in
        eval (Env.bind x v env) body
   | App(f, arg) ->
       (match eval env f with
        | VFun(param, body, closure_env) ->
            let v = eval env arg in
            eval (Env.bind param v closure_env) body
        | _ -> raise (TypeError "App: not a function"))
