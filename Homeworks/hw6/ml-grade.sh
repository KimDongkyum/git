name=hw${HW}_${TASK}
ocamlc commonGrade.ml
ocamlc $name.ml
ocaml commonGrade.cmo $name.cmo ${name}_grade.ml

