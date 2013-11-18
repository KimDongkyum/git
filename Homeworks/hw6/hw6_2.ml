type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let rec parenize (t: tourna): string =
  match t with
    |LEAF v -> v
    |NODE (l, r) -> "(" ^ (parenize l) ^ " " ^ (parenize r) ^ ")"

let ex=parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil));;
