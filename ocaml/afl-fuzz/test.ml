module Interval32 =
struct
  open Int64

  type t = (int64 * int64) option

  let min_int, max_int = min_int, max_int

  let top () = Some (min_int, max_int)
  let bot () = None
  let is_top x = x=top ()
  let is_bot = function None -> true | _ -> false

  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) y = x=y
  let compare (x:t) y = Pervasives.compare x y

  let norm = function None -> None | Some (x,y) ->
    if Int64.compare x y > 0 then None
    else if Int64.compare min_int x > 0 || Int64.compare max_int y < 0 then top ()
    else Some (x,y)

  let (@@) f x = f x

  let equal x y =
    match x, y with
    | None, None -> true
    | Some (x1,x2), Some (y1,y2) -> Int64.compare x1 y1 = 0 && Int64.compare x2 y2 = 0
    | _ -> false

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (x1,x2), Some (y1,y2) -> Int64.compare x1 y1 >= 0 && Int64.compare x2 y2 <= 0

  let join (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (min x1 y1, max x2 y2)

  let meet (x:t) y =
    match x, y with
    | None, z | z, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (max x1 y1, min x2 y2)

  let is_int = function Some (x,y) when Int64.compare x y = 0 -> true | _ -> false
  let of_int x = norm @@ Some (x,x)
  let to_int = function Some (x,y) when Int64.compare x y = 0 -> Some x | _ -> None

  let of_interval (x,y) = norm @@ Some (x,y)

  let of_bool = function true -> Some (Int64.one,Int64.one) | false -> Some (Int64.zero,Int64.zero)
  let is_bool = function None -> false | Some (x,y) ->
    if Int64.compare x Int64.zero = 0 && Int64.compare y Int64.zero = 0 then true
    else not (leq (of_int Int64.zero) (Some (x,y)))
  let to_bool = function None -> None | Some (x,y) ->
    if Int64.compare x Int64.zero = 0 && Int64.compare y Int64.zero = 0 then Some false
    else if leq (of_int Int64.zero) (Some (x,y)) then None else Some true

  let starting n = norm @@ Some (n,max_int)
  let ending   n = norm @@ Some (min_int,n)
  let maximal = function None -> None | Some (x,y) -> Some y
  let minimal = function None -> None | Some (x,y) -> Some x

  let to_excl_list _ = None
  let of_excl_list t _ = top ()
  let is_excl_list _ = false

  let widen x y =
    match x, y with
    | None, z | z, None -> z
    | Some (l0,u0), Some (l1,u1) ->
      let l2 = if Int64.compare l0 l1 = 0 then l0 else min l1 min_int in
      let u2 = if Int64.compare u0 u1 = 0 then u0 else max u1 max_int in
      norm @@ Some (l2,u2)

  let narrow x y =
    match x, y with
    | _,None | None, _ -> None
    | Some (x1,x2), Some (y1,y2) ->
      let lr = if Int64.compare min_int x1 = 0 then y1 else x1 in
      let ur = if Int64.compare max_int x2 = 0 then y2 else x2 in
      norm @@ Some (lr,ur)

  let log f i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match to_bool i1, to_bool i2 with
      | Some x, Some y -> of_bool (f x y)
      | _              -> top ()

  let logor  = log (||)
  let logand = log (&&)

  let log1 f i1 =
    if is_bot i1 then
      bot ()
    else
      match to_bool i1 with
      | Some x -> of_bool (f x)
      | _      -> top ()

  let lognot = log1 not

  let bit f i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try of_int (f x y) with Division_by_zero -> top ())
      | _              -> top ()

  let bitxor = bit Int64.logxor
  let bitand = bit Int64.logand
  let bitor  = bit Int64.logor

  let bit1 f i1 =
    if is_bot i1 then
      bot ()
    else
      match to_int i1 with
      | Some x -> of_int (f x)
      | _      -> top ()

  let bitnot = bit1 Int64.lognot
  let shift_right = bit (fun x y -> Int64.shift_right x (Int64.to_int y))
  let shift_left  = bit (fun x y -> Int64.shift_left  x (Int64.to_int y))
  let rem  = bit Int64.rem

  let neg = function None -> None | Some (x,y) -> norm @@ Some (Int64.neg y, Int64.neg x)
  let add x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (Int64.add x1 y1, Int64.add x2 y2)

  let sub i1 i2 = add i1 (neg i2)

  let mul x y =
    match x, y with
    | None, _ | _, None -> bot ()
    | Some (x1,x2), Some (y1,y2) ->
      let x1y1 = (Int64.mul x1 y1) in let x1y2 = (Int64.mul x1 y2) in
      let x2y1 = (Int64.mul x2 y1) in let x2y2 = (Int64.mul x2 y2) in
      norm @@ Some ((min (min x1y1 x1y2) (min x2y1 x2y2)),
                    (max (max x1y1 x1y2) (max x2y1 x2y2)))

  let rec div x y =
    match x, y with
    | None, _ | _, None -> bot ()
    | Some (x1,x2), Some (y1,y2) ->
      begin match y1, y2 with
        | 0L, 0L       -> bot ()
        | 0L, _        -> div (Some (x1,x2)) (Some (1L,y2))
        | _      , 0L  -> div (Some (x1,x2)) (Some (y1,(-1L)))
        | _ when leq (of_int 0L) (Some (y1,y2)) -> top ()
        | _ ->
          let x1y1n = (Int64.div x1 y1) in let x1y2n = (Int64.div x1 y2) in
          let x2y1n = (Int64.div x2 y1) in let x2y2n = (Int64.div x2 y2) in
          let x1y1p = (Int64.div x1 y1) in let x1y2p = (Int64.div x1 y2) in
          let x2y1p = (Int64.div x2 y1) in let x2y2p = (Int64.div x2 y2) in
          norm @@ Some ((min (min x1y1n x1y2n) (min x2y1n x2y2n)),
                        (max (max x1y1p x1y2p) (max x2y1p x2y2p)))
      end
  let ne i1 i2 = sub i1 i2

  let eq i1 i2 =
    match to_bool (sub i1 i2) with
    | Some x -> of_bool (not x)
    | None -> None

  let ge x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare y2 x1 <= 0 then of_bool true
      else if Int64.compare x2 y1 < 0 then of_bool false
      else top ()

  let le x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare x2 y1 <= 0 then of_bool true
      else if Int64.compare  y2 x1 < 0 then of_bool false
      else top ()

  let gt x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare  y2 x1 < 0 then of_bool true
      else if Int64.compare x2 y1 <= 0 then of_bool false
      else top ()

  let lt x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare x2 y1 < 0 then of_bool true
      else if Int64.compare y2 x1 <= 0 then of_bool false
      else top ()
end

open Interval32

let _ =
  let read () =
    try match read_line () with
      | "bot" -> bot ()
      | "top" -> top ()
      | x -> Int64.of_string x |> of_int
    with _ -> exit 0
  in
  let x = read () in
  let y = read () in

  assert (leq (bot ()) x);
  assert (leq x (top ()));

  assert (leq x (join x y));
  assert (leq y (join x y));
  assert (leq (meet x y) x);
  assert (leq (meet x y) y);

  assert (leq y (widen x y));
  assert (leq (narrow x y) x);

  assert (leq (join x y) (widen x y));
  assert (leq (narrow (meet x y) y) (meet x y));

  assert (not (leq x y && leq y x) || equal x y);

  assert (equal (join x y) (join y x));
  assert (equal (meet x y) (meet y x));
