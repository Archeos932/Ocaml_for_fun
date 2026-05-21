module type StackSig = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t )option
  val is_empty : 'a t -> bool
end


module Stack : StackSig = struct
  type 'a t = 'a list
  let empty = []
  let push x p = x :: p
  let pop = function
    | [] -> None
    | x :: s -> Some (x, s)
  let is_empty = function
    | [] -> true
    | _ -> false
end ;;

module type PRINTABLE = sig
  type t
  val print : t -> unit
end
;
module MakeStack : PRINTABLE = struct
  let push_and_print x s =
    P.print x;
    x :: s
end

(* utilisation *)
module IntStack = MakeStack(struct
  type t = int
  let print = print_int
end)


module type DictSig = sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
  val find : string -> 'a t -> 'a
end

module Dict : DictSig = Map.Make(String)
