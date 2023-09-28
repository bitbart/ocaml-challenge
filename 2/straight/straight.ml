type suit = S | H | D | C;;
type card = Card of int * suit;;

let rndSuit () = match Random.int 4 with
    0 -> S
  | 1 -> H
  | 2 -> D
  | 3 -> C
  | _ -> failwith "cannot happen"
;;

let rndCard () = Card (1 + Random.int 10, rndSuit());;
                
let rndHand () = (rndCard(),rndCard(),rndCard(),rndCard(),rndCard());;

let existsD k m (n1,n2,n3,n4,n5) =
  n1-m=k || n2-m=k || n3-m=k || n4-m=k || n5-m=k
;;

let straight (Card(n1,s1),Card(n2,s2),Card(n3,s3),Card(n4,s4),Card(n5,s5)) =
  let m = min n1 (min n2 (min n3 (min n4 n5)))
  in existsD 1 m (n1,n2,n3,n4,n5) &&
     existsD 2 m (n1,n2,n3,n4,n5) &&
     existsD 3 m (n1,n2,n3,n4,n5) &&
     existsD 4 m (n1,n2,n3,n4,n5)
;;

straight (rndHand());;
straight (Card(5,S),Card(4,H),Card(7,D),Card(6,C),Card(3,C));;
straight (Card(5,S),Card(4,H),Card(7,D),Card(6,C),Card(8,C));;
straight (Card(5,S),Card(4,H),Card(7,D),Card(6,C),Card(2,C));;

let test tot =
  let rec test_rec n tot w =
    if n=0 then (string_of_float ((100. *. (float_of_int w)) /. (float_of_int tot))) ^ "%"
    else test_rec (n-1) tot (w + if straight (rndHand()) then 1 else 0)
in test_rec tot tot 0
;;

test 100000;;
