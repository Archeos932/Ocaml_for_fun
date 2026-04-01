module Arith =  struct

  type expr =
    | Int of int
    | Add of expr * expr
    | Mul of expr * expr

  let rec eval (expr : expr) : int = match expr with
    | Int n -> n
    | Add (e1 , e2) -> (eval e1) + (eval e2)
    | Mul (e1, e2) -> (eval e1) * (eval e2)



  let rec to_string (expr: expr) : string = match expr with
    | Int n -> string_of_int n
    | Add (e1 , e2) -> "(" ^ to_string e1 ^ "+" ^ to_string e2 ^ ")"
    | Mul (e1 , e2) -> "(" ^ to_string e1 ^ "*" ^ to_string e2 ^ ")"


  let rec simplify (expr : expr) : expr = match expr with
    | Int n -> Int n
    | Add (x , Int 0) | Add (Int 0, x) -> simplify x
    | Mul (x , Int 0) | Mul (Int 0, x) -> Int 0
    | Mul (x , Int 1) | Mul (Int 1, x) -> x
    | Mul (_ , _) -> expr
    | Add (_ , _) -> expr

  let a = (Add (Mul (Int 5, Int 4), Int 3)) ;;
  print_string (to_string a) ;;
  Printf.printf "%s \n" "Evalutaion : " ;;
  let a = eval a ;;
    print_int a ;;

end
