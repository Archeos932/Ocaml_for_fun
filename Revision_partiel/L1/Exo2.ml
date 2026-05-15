(*Arbres Binaires*)

type 'a arbre =
  | Leaf
  | Node of 'a arbre * 'a * 'a arbre

let rec hauteur (a:'a arbre) : int  = match a with
  | Leaf              -> 1
  | Node (x,v,y)      -> 1 + max (hauteur x) (hauteur y)

let rec taille = function
  | Leaf -> 0
  | Node (x,v,y)     -> 1 + (taille x) + (taille y)

let rec inorder = function
  | Leaf             -> []
  | Node (g, v, d)   -> inorder g @ [v] @ inorder d

let rec appartient x = function
  | Leaf -> false
  | Node (g,v,d)     -> if (v = x) then true else if (x < v) then (appartient x g) else (appartient x d )

let rec inserer x = function
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (g,v,d) -> if x = v then Node (g, v, d) else
    if (x < v) then Node (inserer x g, v,d ) else  (Node ( g, v, inserer x d ))


let a = Leaf ;;
inserer 5 a ;;
inserer 3 a;;
inserer 7 a;;
inserer 1 a;;
inserer 4 a;;
(*
let abr =
  List.fold_left (fun acc x -> inserer x acc)
    Leaf [5; 3; 7; 1; 4]

let () =
  List.iter (Printf.printf "%d ") (inorder abr)
  (* 1 3 4 5 7 — trié ! *)

*)
type expr = Const of int | Add of expr * expr | Mul of expr * expr | Neg of expr | Var of string

let a = Mul (Add(Var "x", Const 2),Add(Var "x", Neg(Const 1)))

let rec to_string = function
  | Const n -> string_of_int n
  | Var n -> n
  | Neg e -> "(-" ^ to_string e ^ ")"
  | Add (x,y) -> "(" ^ (to_string x) ^ "+" ^ (to_string y) ^")"
  | Mul (x,y) -> "(" ^ (to_string x) ^ "*" ^ (to_string y) ^ ")"

let () = print_endline (to_string a) ;;

let rec eval_basique (a:int) = function
  | Const n -> n
  | Var n -> a
  | Neg e -> -1 * (eval_basique a e)
  | Add (x,y) -> (eval_basique a x) + (eval_basique a y)
  | Mul (x,y) -> (eval_basique a x) * (eval_basique a y)

let rec eval (a : (string * int) list) = function
  | Const n -> n
  | Var n -> List.assoc n a
  | Neg e -> -1 * (eval a e)
  | Add (x,y) -> (eval a x) + (eval a y)
  | Mul (x,y) -> (eval a x) * (eval a y)
;;
print_int (eval [("x", 3)] a) ;;


let rec simplifier = function
  | Add (Const 0, e) | Add (e, Const 0) -> simplifier e
  | Mul (Const 0, e) | Mul (e, Const 0) -> Const 0
  | Mul (Const 1, e) | Mul (e, Const 1) -> simplifier e
  | Neg (Neg e) -> ( simplifier e)
  | Add (a,b) -> Add (simplifier a, simplifier b)
  | Mul (a,b) -> Mul (simplifier a, simplifier b)
  | Neg e -> Neg (simplifier e)
  | e -> e

(*
let () =
  let e = Mul (Add (Var "x", Const 0), Const 1) in
  print_endline (to_string (simplifier e))
  (* "x" *)
*)
