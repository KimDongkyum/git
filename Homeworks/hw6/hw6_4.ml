type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec cal (e: expr): int =
  match e with
    | NUM v -> v
    | PLUS (l,r) -> (cal l)+(cal r)
    | MINUS (l,r) -> (cal l)-(cal r)

let rec eval (f: formula): bool =
  match f with
    | TRUE -> true
    | FALSE -> false
    | NOT v -> (not (eval v))
    | ANDALSO (l,r) -> (eval l) && (eval r)
    | ORELSE (l,r) -> (eval l) || (eval r)
    | IMPLY (l,r) -> (not (eval l) || (eval r))
    | LESS (l,r) -> if (cal l)<(cal r) then true else false
