module type LUHN = sig
  val is_valid : string -> bool
  val identify_network : string -> string
end

module Luhn : LUHN = struct
  let double_and_fix x =
    let a = 2 * x in if a > 9 then a - 9 else a

  let to_int_list str =
    str |> String.to_seq |> Seq.filter (fun c -> c <> ' ')
    |> List.of_seq |> List.map (fun c -> Char.code c - Char.code '0')

  let checksum digits =
    let rec aux is_second l =
      match l with
      | [] -> 0
      | h :: t ->
          if is_second then double_and_fix h + aux false t
          else h + aux true t
    in
    digits |> List.rev |> aux false

  let is_valid s =
    let d = to_int_list s in d <> [] && (checksum d) mod 10 = 0

  let identify_network s =
    let clean = s |> String.to_seq |> Seq.filter (fun c -> c <> ' ') |> List.of_seq in
    match clean with
    | '4' :: _ -> "Visa"
    | '5' :: _ -> "Mastercard"
    | '3' :: _ -> "Amex"
    | _ -> "Inconnu"
end
