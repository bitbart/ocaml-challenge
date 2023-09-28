let dice p6 =
  assert (p6>=0 && p6<=100);
  let n = 1 + Random.int(100) in
  if n<=p6 then 6
  else 1 + Random.int(5)
;;

let test p tot =
  let rec test_rec p n tot n6 =
    if n=0 then string_of_float (100. *. (float_of_int n6) /. (float_of_int tot)) ^ "%"
    else test_rec p (n-1) tot (n6 + if dice p = 6 then 1 else 0)
in test_rec p tot tot 0
;;

test 80 10000;;
