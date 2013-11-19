type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (e: ae) (x: string): ae =
  match e with
    | CONST v -> CONST 0
    | VAR s -> if s=x then CONST 1 else CONST 0
    | POWER (l,r) -> if l=x then TIMES [CONST r; POWER (l,r-1)] else CONST 0
    | TIMES lst-> 
	(match lst with
	   | head::tail -> (SUM [(TIMES (append [(diff head x); []] tail)); (TIMES (append [head; []] (diff tail x)))]))
    | SUM lst ->
	(match lst with
	   | head::tail -> (append (diff head x) (diff tail x)))
