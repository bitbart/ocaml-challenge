let bounce n = fun x -> let y = x mod n in if x mod (2*n) < n then y else n - y;;

assert(bounce 3 10 = 2);;
assert(bounce 4 10 = 2);;
assert(bounce 5 10 = 0);;
assert(bounce 6 10 = 2);;
assert(bounce 7 10 = 4);;
