module StringSet = Set.Make(String)

let s = StringSet.of_list ["x";"y";"z"]
let s2 = StringSet.of_list ["x";"y";"z"]
let a = StringSet.mem "y" s


module StringMap = Map.Make(String)

let count_words words  =
  List.fold_left (fun acc w ->
    let n = match StringMap.find_opt w acc with
      | None -> 0 | Some n -> n in
    StringMap.add w (n+1) acc
  ) StringMap.empty words


let merge m1 m2 =
  StringMap.union (fun _ a b -> Some (a + b)) m1 m2
