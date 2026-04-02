open Core
open Async

let base_price = 100
let current_book = ref Book.empty
let order_count = ref 0

let generate_order () =
  Int.incr order_count;
  let side = if Random.bool () then Types.Buy else Types.Sell in

  let price =
    match !current_book.bids, !current_book.asks with
    | (best_bid :: _), (best_ask :: _) ->
        if Poly.equal side Buy then
          best_ask.price - (Random.int 2) (* L'acheteur tente le prix du vendeur ou un peu moins *)
        else
          best_bid.price + (Random.int 2) (* Le vendeur tente le prix de l'acheteur ou un peu plus *)
    | _ ->
        base_price + (Random.int 11 - 5)
  in

  let qty = 1 + Random.int 10 in
  let trader = if Poly.equal side Buy then "Buyer_" ^ Int.to_string !order_count
               else "Seller_" ^ Int.to_string !order_count in

  { Types.id = !order_count; price; qty; side; trader }

let run_market_loop () =
  let interval = Time_ns.Span.to_span_float_round_nearest (Time_ns.Span.of_ms 500.0) in

  Clock.every interval (fun () ->
    let new_order = generate_order () in

    let start_t = Time_ns.now () in
    let (next_book, trades) = Book.process_order !current_book new_order [] in
    let end_t = Time_ns.now () in
    let diff = Time_ns.diff end_t start_t in

    current_book := next_book;

    if not (List.is_empty trades) then begin
      List.iter trades ~f:(fun t ->
        printf "[MATCH] %s -> %s | %d unités @ %d€ (latence: %s)\n%!"
          t.maker_id t.taker_id t.qty t.price (Time_ns.Span.to_string_hum diff)
      )
    end else begin
      printf "[INFO] %s pose un ordre à %d€ (En attente)\n%!"
        new_order.trader new_order.price
    end;

    match !current_book.bids, !current_book.asks with
    | (b :: _), (a :: _) ->
        printf "[MARKET] Bid: %d€ | Ask: %d€ | Spread: %d€\n----------------------------------------\n%!"
          b.price a.price (a.price - b.price)
    | _ ->
        printf "[MARKET] Recherche de liquidité...\n----------------------------------------\n%!"
  )

let () =
  Random.self_init ();
  printf "=== Démarrage de l'Exchange OCaml (Jane Street Style) ===\n%!";

  run_market_loop ();

  never_returns (Scheduler.go ())
