type winner = Player | Computer | Tie ;;

let win(hp,gp) =
  assert(0<=hp && hp<=5 && 0<=gp && gp<=10);
  let hc = Random.int(6) in 
  let gc = hc + Random.int(6)
  in ((hc,gc), match (hp+hc = gp, hp+hc = gc) with
    (true,false) -> Player
  | (false,true) -> Computer
  | _ -> Tie)
;;

win(2,5);;
