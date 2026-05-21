let t = Array.make 3 (ref 0) in
t.(0) := 42;
print_int !(t.(2))
(*Que vaut t.(2) ? t.(2) vaut 42*)

(*Réécrire le code précédent pour que chaque case du tableau ait son propre ref indépendant.*)

let t = Array.init 4 (fun i -> ref 0)


(*Que vaut sum_leaves arbre après le code suivant ? Expliquer le partage mémoire.*)

type ('n,'f) tree =
  | Leaf of 'f
  | Node of ('n,'f) tree * 'n * ('n,'f) tree


let sub = Leaf (ref 0) in
let arbre = Node (sub, (), sub) in
(match sub with Leaf r -> r := 10 | _ -> ());
(* sum_leaves additionne les valeurs des refs *)
let rec sum_leaves = function
  | Leaf r -> !r
  | Node (l,_,r) -> sum_leaves l + sum_leaves r
