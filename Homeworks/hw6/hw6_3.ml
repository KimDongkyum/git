type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let string_of_team (t:team) : string=
 match t with
   | Korea -> "Korea"
   | France -> "France"
   | Usa -> "Usa"
   | Brazil -> "Brazil"
   | Japan -> "Japan"
   | Nigeria -> "Nigeria"
   | Cameroon -> "Cameroon"
   | Poland -> "Poland"
   | Portugal -> "Portugal"
   | Italy -> "Italy"
   | Germany -> "Germany"
   | Norway -> "Norway"
   | Sweden -> "Sweden"
   | England -> "England"
   | Argentina -> "Argentina"

let rec parenize (t: tourna): string =
  match t with
    | LEAF v -> string_of_team v
    | NODE (l, r) -> "(" ^ (parenize l) ^ " " ^ (parenize r) ^ ")"

let rec drop (t: tourna) (d: team): string =
  match t with
    | LEAF v ->
	if d=v then ""
	else parenize t
    | NODE (l,r) ->
	if (drop l d)="" then (drop r d)
	else if (drop r d)="" then (drop l d)
	else "(" ^ (drop l d) ^ " " ^ (drop r d) ^ ")"
