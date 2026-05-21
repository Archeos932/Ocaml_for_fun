type ('n , 'f) tree =
  | Leaf of 'f
  | Node of 'n * ('n , 'f) tree * ('n , 'f) tree


let leaf f= Leaf f
let noeud (n,g,d)= Node (n,g,d)


let rec max_leaf = function
  | Leaf f -> f
  | Node (_, g, d) -> max (max_leaf g) (max_leaf d)

let rec iter_nodes f = function
  | Leaf x -> f x
  | Node (v,g,d) -> iter_nodes f g ; f v ; iter_nodes f d


(* ça ressemble a du postfixe*)

let rec sums = function
  | Leaf x -> x
  | Node (_, g, d) ->
       let sg = sums g in
       let sd = sums d in
       let s = sum sg + sum sd in
       Node (s, sg, sd)

and sum = function
  | Leaf f -> f
  | Node (s, _, _) -> s



module type Dictsig = sig
  type 'a t
  val empty  : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
  val find : string -> 'a t -> 'a

end ;;

module Dict : Dictsig =
  struct
    type 'a t = (string * 'a) list
    let empty = []
    let add x y t = (x , y) :: t
    let find_opt x d = List.assoc_opt x d
    let find k d = List.assoc k d
  end
  ;;
