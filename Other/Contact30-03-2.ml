module Contact = struct
  type contact = (string * int * int) option

  let ajouter (c:contact) (l: contact list) : contact list = match l with
    | [] -> c :: l
    | x :: y -> c :: x :: y

  let rec chercher (s:string) (l:contact list) : contact = match l with
    | [] -> None
  | x::y -> match x with
    | Some (s,_,_) -> x
    | _ -> chercher s y

  let majeurs l = List.filter (fun c ->
    match c with
    | Some (_, _, i) -> i >= 18
    | None -> false
  ) l

  let print_contact (c : contact) =
    match c with
    | Some (nom, tel, age) ->
        Printf.printf "Nom: %s | Tel: %d | Age: %d\n" nom tel age
    | None ->
        print_endline "Contact inexistant"

  let rec print_list_contact (l: contact list) =
    match l with
      | [] -> print_string "."
      | x::y -> (print_contact x) ; (print_list_contact y)
end
