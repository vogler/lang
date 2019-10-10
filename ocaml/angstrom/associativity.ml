#require "angstrom";;
open Angstrom

let parens p = char '(' *> p <* char ')'
type op = Add | Sub | Mul | Div
type e = Const of int | Binop of op * e * e
let add = char '+' *> return (fun x y -> Binop (Add, x, y))
let sub = char '-' *> return (fun x y -> Binop (Sub, x, y))
let mul = char '*' *> return (fun x y -> Binop (Mul, x, y))
let div = char '/' *> return (fun x y -> Binop (Div, x, y))
let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| fun x -> Const (int_of_string x)

let chainl1 lp op =
  let rest = fix (fun rest -> return (fun x -> (rest >>= fun r -> (op <*> (return x) <*> lp) >>= fun f -> r f) <|> return x)) in
  lp >>= fun l -> rest >>= fun r -> r l

let chainr1 lp op =
  fix (fun rp ->
    lp >>= fun l -> (op <*> (return l) <*> rp) <|> return l)

let expr =
  fix (fun expr ->
    let factor = parens expr <|> integer in
    let term   = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub))

let eval str =
  match parse_only expr (`String str) with
  | Result.Ok v      -> v
  | Result.Error msg -> failwith msg

let _ = eval "1+2+3"
