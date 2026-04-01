exception VariableNonDefinie of string
exception DivisionParZero
exception NulValue of int

type binop = | Add | Sub | Mul | Div

type expr =
  | Int of int
  | Var of string
  | Binop of binop * expr * expr
  | Let of string * expr * expr

type env = (string * int) list


let rec lookup (s:string) (e:env) : int option =
  match e with
  | [] -> None
  | (x,y) :: z -> if (x = s) then Some y else (lookup s z)


let rec eval (e : expr) (env : env) : int = match e with
  | Int n -> n
  | Var s -> (match lookup s env with
    | Some y -> y
    | None -> raise (VariableNonDefinie s))
  | Binop (op, e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match op with
      | Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> if (v2 = 0) then raise (DivisionParZero) else v1 / v2
    )
  | Let (nom, e1, e2) ->
        let valeur_e1 = eval e1 env in
        eval e2 ((nom, valeur_e1) :: env)

let rec repl () =
  print_string "OcamlCalc > " ;
  flush stdout;

  let res = read_line () in
  if (res = "exit") then print_endline "Bye"
  else begin
    Printf.printf "Tu as envoyé : %s\n" res;
    repl () ;
  end


let () = repl () ;;
