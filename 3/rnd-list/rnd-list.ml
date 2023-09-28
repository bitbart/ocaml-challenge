let rec rnd_list n b = 
  if n = 0 then []
  else 1+(Random.int b) :: rnd_list (n-1) b
;;

rnd_list 10 5;;
