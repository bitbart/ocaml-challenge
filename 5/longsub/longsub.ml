let longsub l =
  let rec longsub_rec (x,nx) (xmax,nxmax)  = function
      [] -> if nx>nxmax then Some (x,nx) else Some (xmax,nxmax)
    | y::l -> if x=y then longsub_rec (y,nx+1) (xmax,nxmax) l
              else let (x',nx') = if nx>nxmax then (x,nx) else (xmax,nxmax)
                   in match longsub_rec (y,1) (x',nx') l with
                        None -> Some (x',nx')
                      | Some (x',nx') ->
                         if nx'>nxmax then Some (x',nx') else Some (xmax,nxmax)
  in match l with
       [] -> None
     | x::l' -> longsub_rec (x,1) (x,1) l'
;;

longsub [1;1;2;2;2;1;1;1;1];;

longsub [1;1;2;2;3;3;3;3;1;1;4;4;4;4;5;5;5;5;5;5;1;1];;
