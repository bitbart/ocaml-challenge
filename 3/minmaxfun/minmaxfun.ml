let rec minmaxfun f a b = 
  if a>b then None
  else match minmaxfun f (a+1) b with
      None -> Some (f a,f a)
    | Some (n,m) ->
      let n' = if (f a) < n then f a else n
      and m' = if (f a) > m then f a else m
      in Some (n',m')
;;

let rec range a b = if a>b then [] else a::(range (a+1) b);;

List.map (minmaxfun (fun n -> n * n * n) (-2)) (range (-5) 5);;
List.map (minmaxfun (fun n -> n * n * n) (2)) (range (0) 5);;
