type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec multiply_diff (f: ae -> ae) (l: ae list): ae =
    match l with
      | [] -> CONST 0
      | head::tail -> 
	 (if tail=[] then (f head)
	  else SUM (List.append [TIMES (List.append [(f head)] tail)] [TIMES [head; (multiply_diff f tail)]]))

let rec my_diff (x: string) (e: ae): ae =
  match e with
    | CONST v -> CONST 0
    | VAR s -> if s=x then CONST 1 else CONST 0
    | POWER (l,r) -> if l=x then TIMES [CONST r; POWER (l,r-1)] else CONST 0
    | TIMES lst -> (multiply_diff (my_diff x) lst) 
    | SUM lst -> SUM (List.map (my_diff x) lst)

let rec diff (a: ae) (b: string): ae = (my_diff b a)
