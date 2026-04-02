open Base
open Types

let match_orders (o1: order) (o2: order) : int option =
  (* On part du principe que o1 = Buy et o2 = Sell *)
  if o1.price >= o2.price then
    (* On retourne la quantité échangée (le minimum des deux) *)
    Some (Int.min o1.qty o2.qty)
  else
    (* Trop cher pour l'acheteur, pas de match *)
    None
