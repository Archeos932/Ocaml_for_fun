type grille = int array array
type pos = int * int

let est_valide (g:grille) (x:int) (l:int) (c:int) : bool =
  let valide = ref true in
  for i = 0 to 8 do
    if (g.(l).(i) = x || g.(i).(x) = x) then valide := false
  done ;
  let debut_l, debut_c = (l/3)*3,(c/3)*3 in
  for i = 0 to 2 do
    for j = 0 to 2 do
if (g.(debut_l + i).(debut_c + j) = x) then valide :=false
    done
  done ;
  !valide


let rec trouve_vide g l c =
  if l = 9 then None
  else if c = 9 then trouve_vide g (l + 1) 0
  else if g.(l).(c) = 0 then Some (l, c)
  else trouve_vide g l (c + 1)


let rec solveur g =
  match trouve_vide g 0 0 with
  | None -> true
  | Some (l, c) ->
      let rec tester_chiffre v =
        if v > 9 then false
        else if est_valide g l c v then
          begin
            g.(l).(c) <- v;
            if solveur g then true
            else (g.(l).(c) <- 0; tester_chiffre (v + 1))
          end
        else tester_chiffre (v + 1)
      in
      tester_chiffre 1
