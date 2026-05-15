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
let check_non_vide t =
  if Array.length t = 0
  then invalid_arg "tableau vide"

let minimum t =
  check_non_vide t ;
  let m = ref t.(0) in
  for i = 1 to (Array.length t) -1  do
    if (t.(i) < !m) then m := t.(i)
  done ; m

let macimum t =
  check_non_vide t ;
  let m = ref t.(0) in
  for i = 1 to (Array.length t) -1  do
    if (t.(i) >= !m) then m := t.(i)
  done ; m


let somme t =
  check_non_vide t ;
  let acc = ref 0 in
  for i = 0 to (Array.length t ) -1 do
    acc := !acc + t.(i)
  done ; !acc

let moyenne t =
  check_non_vide t;
  float_of_int (somme t) /. float_of_int (Array.length t)


let tri_insertion (t:int array) =
  let n = Array.length t in
  for i = 1 to (n-1) do
    let cle = t.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && t.(!j) > cle do
    t.(!j + 1) <- t.(!j) ;
    decr j ;
    done ;
    t.(!j + 1) <- cle
  done


let () =
  let t = [|5; 3; 8; 1; 4|] in
  tri_insertion t;
  Array.iter (Printf.printf "%d ") t
  (* 1 3 4 5 8 *)
