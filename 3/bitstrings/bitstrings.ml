type bitstring = E | Z of bitstring | U of bitstring;;

let rec string_of_bitstring = function
    E -> ""
  | Z s -> "0" ^ string_of_bitstring s
  | U s -> "1" ^ string_of_bitstring s
;;

let rec len = function
    E -> 0
  | U s | Z s -> 1 + len s
;;

let rec exp0 = function
    0 -> E
  | n when n>0 -> Z (exp0 (n-1))
  | n -> failwith "exp0 n requires n>=0"
;;

exp0 3;;

let rec countZ = function
    E -> 0
  | Z s -> 1 + countZ s
  | U s -> countZ s
;;

countZ (Z(Z(Z(U(Z(U (Z E)))))));;

let rec concat s1 s2 = match s1 with
    E -> s2
  | Z s1' -> Z(concat s1' s2)
  | U s1' -> U(concat s1' s2)               
;;

assert(let s = Z(Z(Z(U(Z(U (Z E)))))) in len (concat s s) = 2 * len s);;

let rec equals s1 s2 = match (s1,s2) with
    (E,E) -> true
  | (Z s1',Z s2') -> equals s1' s2'
  | (U s1',U s2') -> equals s1' s2'
  | _ -> false
;;

assert(let s = Z(Z(Z(U(Z(U (Z E)))))) in equals s s);;

let rec tl = function
    E -> E
  | Z s -> s
  | U s -> s
;;
         
let rec prefix s1 s2 = match (s1,s2) with
    (E,_) -> true
  | (Z s1',Z s2') -> prefix s1' s2'
  | (U s1',U s2') -> prefix s1' s2'
  | _ -> false
;;

assert(let s = (Z(Z(Z(U(Z E))))) in prefix s (concat s ((Z(Z(Z(U(Z(U(Z E))))))))));;
assert(let s = (Z(Z(Z(U(Z(U (Z E))))))) in prefix s (concat s (concat s s)));;

let rec substring s1 s2 =
  if prefix s1 s2 then true
  else match tl s2 with
      E -> false
    | s2' -> substring s1 s2'
;;

assert(let s = (Z(U(Z E))) in prefix s (concat s ((Z(Z(Z(U(Z(U(Z E))))))))));;
