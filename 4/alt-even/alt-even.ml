let alt_even x =
  let rec alt_even_rec x b = 
    if x=0 then b
    else if (x mod 2 = 0) != b then false else alt_even_rec (x/10) (not b)
  in alt_even_rec x true;;

assert(alt_even 1234);;
assert(not (alt_even 1234567));;
