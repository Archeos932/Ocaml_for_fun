open Base

type side = | Buy | Sell [@@deriving sexp]

type order = {
  id :int ;
  price : int ;
  qty : int ;
  side : side ;
  trader : string ;
} [@@deriving sexp]

type trade = {
  maker_id : string;
  taker_id : string;
  price    : int;
  qty      : int;
} [@@deriving sexp]
