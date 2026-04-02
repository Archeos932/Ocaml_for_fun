open Core
open Async

(* --- Configuration de la simulation --- *)
let base_price = 100
let current_book = ref Book.empty
let order_count = ref 0

(* Création d'un ordre "intelligent" qui suit le marché *)
let generate_order () =
  Int.incr order_count;
  let side = if Random.bool () then Types.Buy else Types.Sell in

  (* Logique pour générer un prix qui a de fortes chances de matcher *)
  let price =
    match !current_book.bids, !current_book.asks with
    | (best_bid :: _), (best_ask :: _) ->
        if Poly.equal side Buy then
          best_ask.price - (Random.int 2) (* L'acheteur tente le prix du vendeur ou un peu moins *)
        else
          best_bid.price + (Random.int 2) (* Le vendeur tente le prix de l'acheteur ou un peu plus *)
    | _ ->
        (* Si le carnet est vide, on oscille autour du prix de base *)
        base_price + (Random.int 11 - 5)
  in

  let qty = 1 + Random.int 10 in
  let trader = if Poly.equal side Buy then "Buyer_" ^ Int.to_string !order_count
               else "Seller_" ^ Int.to_string !order_count in

  { Types.id = !order_count; price; qty; side; trader }

(* --- La boucle de simulation Async --- *)
let run_market_loop () =
  (* On convertit le Span en float pour la compatibilité avec Clock.every *)
  let interval = Time_ns.Span.to_span_float_round_nearest (Time_ns.Span.of_ms 500.0) in

  Clock.every interval (fun () ->
    let new_order = generate_order () in

    (* Mesure de la latence *)
    let start_t = Time_ns.now () in
    let (next_book, trades) = Book.process_order !current_book new_order [] in
    let end_t = Time_ns.now () in
    let diff = Time_ns.diff end_t start_t in

    current_book := next_book;

    (* 1. Affichage des transactions s'il y a un match *)
    if not (List.is_empty trades) then begin
      List.iter trades ~f:(fun t ->
        printf "[MATCH] %s -> %s | %d unités @ %d€ (latence: %s)\n%!"
          t.maker_id t.taker_id t.qty t.price (Time_ns.Span.to_string_hum diff)
      )
    end else begin
      printf "[INFO] %s pose un ordre à %d€ (En attente)\n%!"
        new_order.trader new_order.price
    end;

    (* 2. Affichage de la santé du marché (Spread) *)
    match !current_book.bids, !current_book.asks with
    | (b :: _), (a :: _) ->
        printf "[MARKET] Bid: %d€ | Ask: %d€ | Spread: %d€\n----------------------------------------\n%!"
          b.price a.price (a.price - b.price)
    | _ ->
        printf "[MARKET] Recherche de liquidité...\n----------------------------------------\n%!"
  )

(* --- Point d'entrée --- *)
let () =
  Random.self_init ();
  printf "=== Démarrage de l'Exchange OCaml (Jane Street Style) ===\n%!";

  (* Lancement de la boucle asynchrone *)
  run_market_loop ();

  (* Le Scheduler maintient le programme en vie infiniment *)
  never_returns (Scheduler.go ())
