module Stack = struct

  type pile = (int list)

  let push (i:int) (p:pile) : pile = match p with
    | _ ->  (i :: p)

  let pop (p:pile) : (int * pile) option = match p with
    |  [] -> None
    |  x::y -> Some (x, y)

  let rec somme (p:pile) : int = match p with
    | [] -> 0
    | x :: y  -> x + somme y

  let peek (p:pile) : int option = match p with
    | [] -> None
    | x :: y -> Some x
end
