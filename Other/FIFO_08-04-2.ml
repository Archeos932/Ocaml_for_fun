type 'a fifo = { first : 'a list; last: 'a list };;


let empty = { first = []; last = [] }

let length q = List.length q.first + List.length q.last

let push x q = { q with last = x :: q.last }

let pop q =
  match q.first with
  | x :: rest -> Some (x, { q with first = rest })
  | [] ->
      match List.rev q.last with
      | [] -> None
      | x :: rest -> Some (x, { first = rest; last = [] })

let to_list q = q.first @ List.rev q.last

let from_list l = { first = l; last = [] }

let equal q1 q2 = (to_list q1) = (to_list q2)

let append q1 q2 =
  { first = q1.first;
    last = q2.last @ (List.rev q2.first) @ q1.last }


let rec fold f init q = match pop q with
  | None -> init
  | Some (x, q') -> fold f (f init x ) q'
;;
