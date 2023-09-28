(* TODO: da riprendere con le definizioni di dfa *)

(* Set utility functions *)

let isempty = function
    [] -> true
  | _ -> false
;;

let rec union xl = function
    [] -> xl
  | y::yl -> (if List.mem y xl then [] else [y]) @ (union xl yl)
;;

let rec intersect xl = function
    [] -> []
  | y::yl -> (if List.mem y xl then [y] else []) @ (intersect xl yl)
;;

let rec setminus xl yl = match xl with
    [] -> []
  | x::xl' -> if List.mem x yl then setminus xl' yl else x::(setminus xl' yl)
;;

let rec set_of_list = function
    [] -> []
  | x::l -> if List.mem x l then set_of_list l else x::(set_of_list l)
;;

(* mapflat : ('a -> 'b list) -> 'a list -> 'b list *)
let rec mapflat f = function
  [] -> []
  | x::l -> f x @ mapflat f l
;;

let rec range n m = if m<n then [] else n::(range (n+1) m);;

(* 
  FSA datatype: trans, q0, F
  States are represented as integers: the actual set of states 
  is determined by the transition relation, and therefore it is finite 
*)

type 'a trans = int * 'a * int
type 'a fsa = Fsa of 'a trans list * int * int list;;

let trans (Fsa(ts,q0,qf)) = ts;;

let init (Fsa(ts,q0,qf)) = q0;;

let final (Fsa(ts,q0,qf)) = qf;;

let labels (Fsa(ts,q0,qf)) = set_of_list (List.map (fun (q,l,q') -> l) ts);;

let states (Fsa(ts,q0,qf)) = union
    (union [q0] qf)
    (set_of_list (mapflat (fun (q,a,q') -> [q;q']) ts))
;;

let delta (Fsa(ts,qz,qf)) q a = 
  List.map (fun (q1,q2,q3) -> q3) (List.filter (fun (q1,l,q2) -> q1=q && l=a) ts);;

let deltaSet (Fsa(ts,qz,qf)) qq a = 
  List.map (fun (q1,q2,q3) -> q3) (List.filter (fun (q1,l,q2) -> (List.mem q1 qq) && l=a) ts);;

let rec fix f z = if card (f z) = card z then z else fix f (f z);;

let f m qr qx = union qr (deltaSet m qx Eps);;

let clos m qr = set_of_list (fix (fun qx -> f m qr qx) []);;

let deltaSetEps m qx a = clos m (deltaSet m (clos m qx) a);;

let rec deltaSetStar m qq w = match w with
  [] -> clos m qq
| a::w' -> deltaSetStar m (deltaSetEps m qq a) w'
;;

let accepts (Fsa(ts,qz,qf)) w =
  not (isempty (intersect (deltaSetStar (Fsa(ts,qz,qf)) [qz] w) qf))
;;

let statesSet m qr ts ll = set_of_list ((mapflat (fun qx -> mapflat (fun a -> [qx;deltaSetEps m qx a]) ll)) qr);;

let transSet m qr ts ll = 
  mapflat 
  (fun qx -> 
    (List.map (fun a -> (qx,a,deltaSetEps m qx a)) ll)
  )
  qr;;

let whichwords m l = List.map (fun x -> (string_of_word x,accepts m x)) (List.map word_of_string l);;
