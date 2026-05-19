module type DictSig = sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
  val find : string -> 'a t -> 'a
end

module Dict : DictSig = struct
  type 'a t = (string * 'a) list
  let empty = []
  let add key value d = (key, value) :: d
  let find_opt key d = List.assoc_opt key d
  let find key d =
    match List.assoc_opt key d with
    | Some v -> v
    | None   -> raise Not_found
end


type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

;;


let a = Let ("x" ,Const 2 , Add (Var ("x") , Const 1))
let b  = Add (Mul (Const 3, Const 4), Let ("y", Const 2, Mul (Var ("y"), Var ("y"))))

let rec eval (ctx : int Dict.t) (e : expr) = match e with
  | Const n -> n
  | Add (x, y)->  (eval ctx x) + (eval ctx y )
  | Mul (x, y)->  (eval ctx x) * (eval ctx y )
  | Var x -> Dict.find x ctx
  | Let (x, x_def, body) ->
    let v = eval ctx x_def in
    let ctx' = (Dict.add x v ctx) in
    eval ctx' body


let _ = Printf.printf "%d\n" (eval Dict.empty a)  (* doit afficher 3 *)
let _ = Printf.printf "%d\n" (eval Dict.empty b)  (* doit afficher 16 *)
;;

(*---------------*)
module StringSet = Set.Make(String)

let rec free_vars (e : expr) : StringSet.t = match e with
  | Const n -> StringSet.empty
  | Var x -> StringSet.singleton x
  | Add (e1, e2) | Mul (e1, e2) -> StringSet.union (free_vars e1) (free_vars e2)
  | Let (x, x_def, body) ->
    let body_vars = StringSet.remove x (free_vars body) in
    StringSet.union body_vars (free_vars x_def)


let print_free_vars (e : expr) : unit =
  StringSet.iter (fun x -> print_endline x) (free_vars e)


let _ = print_free_vars (Let("x", Const 2, Add (Var "x", Var "z")))
