let guess n =
  assert (n>=1 && n<=5);
  let r = 1 + Random.int(5)
  in (n=r, r)
;;

guess 5;;
