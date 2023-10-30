let parrot_trouble talking hour =
  if hour<0 || hour>23 then None
  else Some (talking && (hour<7 || hour>20))
;;
