type weekday = Mo | Tu | We | Th | Fr;;
type course = ALF | LIP;;

let isLecture d c = match c with
    ALF -> d=Tu || d=Th || d=Fr
  | LIP -> d=We || d=Th
;;
