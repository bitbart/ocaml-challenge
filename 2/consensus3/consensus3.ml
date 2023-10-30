let consensus3 (f0,f1,f2) =
  fun x -> match (f0 x,f1 x,f2 x) with
      (y0,y1,_) when y0=y1 -> Some y0
    | (y0,_,y2) when y0=y2 -> Some y0
    | (_,y1,y2) when y1=y2 -> Some y1
    | _ -> None
;;

assert(consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);;

assert(consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);;

assert(consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);;

try
  let _ = consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0 in assert(false)
with _ -> assert(true);;
