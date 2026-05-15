let compteur = ref 0

let incrementer () = compteur := !compteur + 1
let reset () = compteur := 0
let valeur () = !compteur

let () =
  for _ = 1 to 5 do incrementer () done ;
  Printf.printf "valeur = %d\n" (valeur ());
  reset ();
  Printf.printf "après reset = %d\n" (valeur ())

let push (x:int) (pile:int list ref) = pile := x :: !pile

let pop (pile: int list ref) : int = match !pile with
  | []     -> failwith "pile vide"
  | x :: y -> pile := y ; x

let peek (pile: int list ref) = match !pile with
  | []     -> failwith "pile vide"
  | x :: _ -> x

let is_empty (pile : int list ref) : bool = match !pile with
  | []     -> true
  | _ :: _ -> false

let () =
  let p = ref [] in
        push 1 p; push 2 p; push 3 p;
        Printf.printf "pop = %d\n" (pop p);   (* 3 *)
        Printf.printf "pop = %d\n" (pop p);   (* 2 *)
        Printf.printf "peek = %d\n" (peek p)

(*---*)
let array_valide (a:int array) : bool = Array.length a != 0
