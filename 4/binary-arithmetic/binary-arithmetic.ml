type bitstring = ZT | UT | Z of bitstring | U of bitstring;;

let rec string_of_bitstring = function
    ZT -> "0"
  | UT -> "1"
  | Z s -> "0" ^ string_of_bitstring s
  | U s -> "1" ^ string_of_bitstring s
;;

let rec len = function
    ZT | UT -> 1
  | U s | Z s -> 1 + len s
;;

let rec pow = function
    ZT | UT -> 1
  | U s | Z s -> 2 * pow s
;;

let rec int_of_bitstring = function
    ZT -> 0
  | UT -> 1
  | Z s -> int_of_bitstring s
  | U s -> pow (U s) + (int_of_bitstring s)
;;

let s = (U (U (Z (U ZT)))) in
(string_of_bitstring s) ^ " -> " ^ string_of_int(int_of_bitstring s)
;;

let rec succ = function
    ZT -> UT
  | UT -> U ZT
  | Z s -> let s' = succ s in
    if len s' = len s then Z s'
    else s'
  | U s -> let s' = succ s in
    if len s' = len s then U s'
    else match s' with
        U s'' -> U (Z s'')
      | _ -> failwith "cannot happen"
;;

let rec range = function
    0 -> [ZT]
  | n -> let l = range (n-1) in (succ (List.hd l))::l
;;

List.map int_of_bitstring (range 20);;

let rec add s1 s2 =
  assert (len s1 = len s2);
  match (s1,s2) with
    (ZT,ZT) -> ZT
  | (ZT,UT) -> UT
  | (UT,ZT) -> UT
  | (UT,UT) -> U ZT
  | (Z s1',Z s2') -> Z(add s1' s2')
  | (Z s1',U s2') -> let s' = add s1' s2' in
    if len s' = len s1' then U s' else (match s' with
          ZT -> Z UT
        | UT -> U ZT
        | Z w -> U (Z w)
        | U w -> U (Z w))
  | (U s1',Z s2') ->      
    let s' = add s1' s2' in
    if len s' = len s1' then U s' else (match s' with
          ZT -> Z UT
        | UT -> U ZT
        | Z w -> U (Z w)
        | U w -> U (Z w))          
  | (U s1',U s2') -> let s' = add s1' s2' in
    if len s' = len s1' then U (Z s') else U s'
  | _ -> failwith "add: arguments must have the same length"
;;

assert(int_of_bitstring (add (Z ZT) (Z ZT)) = 0);;
assert(int_of_bitstring (add (Z UT) (Z ZT)) = 1);;
assert(int_of_bitstring (add (Z UT) (Z UT)) = 2);;
assert(int_of_bitstring (add (U ZT) (U ZT)) = 4);;
assert(int_of_bitstring (add (Z UT) (U UT)) = 4);;
assert(int_of_bitstring (add (U UT) (Z UT)) = 4);;
assert(int_of_bitstring (add (U UT) (U ZT)) = 5);;
assert(int_of_bitstring (add (U ZT) (U UT)) = 5);;
assert(int_of_bitstring (add (U UT) (U UT)) = 6);;
