type unionfind = int array;;

let rec find_1 (u : unionfind) (x : int) =
  if u.(x) = x then x else find_1 u u.(x)

let union_1 (u : unionfind) (x : int) (y : int) =
  let ri = find_1 u x in
  let rj = find_1 u y in
  if ri <> rj then u.(rj) <- ri

let same_class_1 (u : unionfind) (x : int) (y : int) = ((find_1 u x) = (find_1 u y))

(* --- Avec Path compression --- *)

let rec find_2 (u : unionfind) (x : int) =
  if u.(x) = x then x else begin
    let rj = find_2 u u.(x) in
    u.(x) <- rj ;
    rj
  end

let union_2 (u : unionfind) (x : int) (y : int) =
  let rx = find_2 u x in
  let ry = find_2 u y in
  if rx <> ry then u.(ry) <- rx


let same_class_2 (u : unionfind) (x : int) (y : int) = find_2 u x = find_2 u y
