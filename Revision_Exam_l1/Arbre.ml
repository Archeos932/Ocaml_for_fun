type ('n,'f) tree =
  | Leaf of 'f
  | Node of ('n,'f) tree * 'n * ('n,'f) tree

let rec count_leaves = function
  | Leaf _ -> 1
  | Node  (g,v,d) -> (count_leaves g ) + (count_leaves d)

let rec hauteur = function
  | Leaf _ -> 0
  | Node (g,v,d) -> max (1 + hauteur g) (1 + hauteur d)

let rec miroir = function
  | Leaf x -> Leaf x
  | Node (g,v,d) -> Node (miroir g, v, miroir d)

let rec iter_preorder f = function
  | Leaf x -> ()
  | Node (g,v,d) ->
    f v ;
    iter_preorder f g ;
    iter_preorder f d

let rec sum_leaves = function
  | Leaf x -> x
  | Node (g, v, d) -> (sum_leaves g) + (sum_leaves d)


let rec sums = function
  | Leaf f -> (Leaf f, f)
  | Node (l, _, r) ->
      let (l', sl) = sums l in
      let (r', sr) = sums r in
      (Node (l', sl+sr, r'), sl+sr)

let rec make_perfect n x = match n with
  | 0 -> Leaf x
  | n -> Node (make_perfect (n-1) x, "", make_perfect (n-1) x)
