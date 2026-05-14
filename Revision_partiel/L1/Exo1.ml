(*Types & Pattern Matching*)
type forme =
  | Cercle of float
  | Carre of float
  | Rectangle of float * float
  | Triangle of float * float * float

let nom_forme (f:forme) : string = match f with
  | Cercle _     -> "Cercle"
  | Carre _      -> "Carre"
  | Rectangle _  -> "Rectangle"
  | Triangle  _  -> "Triangle"

let perimetre (f:forme) : float = match f with
  | Cercle x -> 2.0 *. x *. Float.pi
  | Carre x -> 4.0 *. x
  | Rectangle (x, y) -> 2.0 *. (x +. y)
  | Triangle (x, y, z) -> x +. y +. z

let aire (f:forme) : float = match f with
  | Cercle x -> x *. x *. Float.pi
  | Carre x -> x *. x
  | Rectangle (x, y) -> x *. y
  | Triangle (a, b, c) ->
        if a +. b <= c || a +. c <= b || b +. c <= a
        then 0.0  (* triangle invalide *)
        else
          let s = (a +. b +. c) /. 2.0 in
          sqrt (s *. (s -. a) *. (s -. b) *. (s -. c))

let rec total_aire (lf: forme list) : float = match lf with
  | [] -> 0.0
  | x :: y -> aire x +. total_aire y

let est_reguliere  (f:forme) : bool = match f with
  |Cercle _ | Carre _ -> false
  | Rectangle (x,y) ->  x = y
  | Triangle (x,y,z) -> x = y && y = z

let homothetie (f:float) = function
  | Cercle x -> Cercle (f *. x)
  | Carre x -> Carre (f *. x)
  | Rectangle (x,y) -> Rectangle (f *. x, f *. y)
  | Triangle (x,y,z) -> Triangle (f *. x, f *. y,f *. z)

let a = Cercle 5.0 ;;
let () = Printf.printf "%s\n" (nom_forme ( a))
let () = print_float  (perimetre ( a))
