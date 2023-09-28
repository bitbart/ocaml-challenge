let hot () =
  let b = Random.int(2)
  in if b=0 then "head" else "tail"
;;

hot();;
hot();;
hot();;

let hot_wrong =
  let b = Random.int(2)
  in if b=0 then "head" else "tail"
;;

hot_wrong;;
hot_wrong;;
hot_wrong;;

