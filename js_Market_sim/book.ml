open Base
open Types

type t = {
  bids : order list;
  asks : order list;
}

let empty = { bids = []; asks = [] }

(** Trie les acheteurs du plus cher au moins cher, et les vendeurs du moins cher au plus cher *)
let add_order (book : t) (new_order : order) : t =
  match new_order.side with
  | Buy ->
      let new_bids = List.sort (new_order :: book.bids) ~compare:(fun a b -> Int.compare b.price a.price) in
      { book with bids = new_bids }
  | Sell ->
      let new_asks = List.sort (new_order :: book.asks) ~compare:(fun a b -> Int.compare a.price b.price) in
      { book with asks = new_asks }

(** Moteur de matching récursif *)
let rec process_order (book : t) (new_order : order) (trades : trade list) : (t * trade list) =
  if new_order.qty <= 0 then (book, trades)
  else
    match new_order.side with
    | Buy ->
        (match book.asks with
         | best_ask :: remaining_asks when new_order.price >= best_ask.price ->
             let traded_qty = Int.min new_order.qty best_ask.qty in
             let current_trade = {
               maker_id = best_ask.trader;
               taker_id = new_order.trader;
               price = best_ask.price;
               qty = traded_qty;
             } in
             let updated_new_order = { new_order with qty = new_order.qty - traded_qty } in
             let new_book =
               if best_ask.qty > traded_qty then
                 { book with asks = { best_ask with qty = best_ask.qty - traded_qty } :: remaining_asks }
               else
                 { book with asks = remaining_asks }
             in
             process_order new_book updated_new_order (current_trade :: trades)

         | _ -> (add_order book new_order, trades))

    | Sell ->
        (match book.bids with
         | best_bid :: remaining_bids when new_order.price <= best_bid.price ->
             let traded_qty = Int.min new_order.qty best_bid.qty in
             let current_trade = {
               maker_id = best_bid.trader;
               taker_id = new_order.trader;
               price = best_bid.price;
               qty = traded_qty;
             } in
             let updated_new_order = { new_order with qty = new_order.qty - traded_qty } in
             let new_book =
               if best_bid.qty > traded_qty then
                 { book with bids = { best_bid with qty = best_bid.qty - traded_qty } :: remaining_bids }
               else
                 { book with bids = remaining_bids }
             in
             process_order new_book updated_new_order (current_trade :: trades)

         | _ -> (add_order book new_order, trades))
