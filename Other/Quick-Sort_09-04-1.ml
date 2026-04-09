let swap array i j = let
  temp = array.(i) in
  array.(i) <- array.(j)
;array.(j) <- temp

let shuffle array =
  let rec aux i =
    if i > 0 then
      let j = Random.int (i + 1) in
      swap array i j ;
      aux ( i - 1)
  in aux (Array.length array - 1)

let partition array first_index last_index pivot_index =
  swap array pivot_index last_index ;
  let j = ref first_index in
  for i = first_index to (last_index - 1) do
    if array.(i) <= array.(last_index) then begin
      swap array i !j ;
      j := !j + 1
    end
  done ;
  swap array last_index !j;
  !j

let choose_pivot array first_index last_index = first_index + (last_index - first_index) / 2

let rec quicksort_sub array first_index last_index =
  if first_index < last_index then
    let cp = choose_pivot array first_index last_index in
    let np = partition array first_index last_index cp in
    quicksort_sub array first_index (np - 1);
    quicksort_sub array (np + 1) last_index


let quicksort tab =
  let n = Array.length tab in
  if n > 1 then quicksort_sub tab 0 (n - 1)
