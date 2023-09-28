let seven_eleven_doubles () =
  let d1 = 1 + Random.int(6) in
  let d2 = 1 + Random.int(6)
  in (d1=d2 || d1+d2=7 || d1+d2=11, d1, d2)
;;

seven_eleven_doubles();;
