(*CE PROGRAMME N'A PAS POUR BUT D'ETRE COMPILÉ C POUR S'ENTRAINER POUR L'EXAM *)

type 'a t1 = A of 'a * bool | B of 'a | C
type 'a t2 = D of 'a * 'a | E of bool * 'a | F of bool
type ('n,'f) ttree =
  | Leaf of 'f
  | Node of 'n * ('n,'f) ttree * ('n,'f) ttree * ('n,'f) ttree

(*Supposons que u est un type à n valeurs distinctes.
Donner le nombre de valeurs distinctes de u t1 et u t2 en fonction de n.*)

(*t1 a donc 2 fois n plus 1 fois n + 1*)
(*t2 a donc n carré + 2n  + 2 *)

(*Non il existe aucune bijection possible vu qu'il n'ont pas le meme cardinal*)

let a = Node ("r" , Leaf 0, Leaf 1 ,  Leaf 2)

type ('n, 'f) tree =
  | Leaf of 'f
  | Node of ('n,'f) tree * 'n * ('n,'f) tree


let leaf x = Leaf x
let noeud g x d : ('n, 'f) tree = (Node g x d )

let rec iter_nodes = function
  | Leaf _ -> ()
  | Node g x d -> 1 + Node g + Node d


let rec iter_nodes f = function
  | Leaf -> ()
  | Node g x d -> iter_nodes f g ; f x ; iter_nodes f d


let a = node (ref 0) (leaf ()) (leaf ()) in
let b = node (ref 0) a a in
iter_nodes (fun n -> n := !n + 1) b;
iter_nodes (fun n -> print_int !n; print_newline ()) b ;

(*affiche 2 1 2*)
(*
let rec sums  = function
  | Leaf x -> x
  | Node (g, _, d) ->
    let sg =  sums g in
    let sd = sums d in
    let s = sum sg + sum sd in
    Node (sg, s, sd)

let sum = function
  | Leaf f -> f
  | Node (_,s,_) -> s

  *)
;;

module type DictSig = sig
  type 'a t
    val empty : 'a t
    val add : string -> 'a -> 'a t  -> 'a t
    val find_opt : string -> 'a t -> 'a option
    val find_ : string -> 'a t -> 'a
end ;;

module Dict: DicSig = struct
  type 'a t
  let empty = []
  let add x y d= (x , y) :: d
  let find_opt k d = List.assoc_opt k d
  let find k d = List.assoc k d
end ;;


(*reecrivont dict avec Map.make*)
module Dict2 = Map.Make(String ) ;
(*avantage O(ln n ) a la place de O(n)*)


(*let d = [] in marche pas, pas du meme type*)
let d = Dict.empty
let d = Dict.add "x" 42 d in
Dict.find_opt "x" d
