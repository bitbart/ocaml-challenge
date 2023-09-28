(**********************************************************************
 * DFAs 
   States are represented as integers, labels are polymorphic
   pre: trans has no duplicates
   pre: init in states(trans)
   pre: for all q in final . q in states(trans)
 **********************************************************************)

type 'a fsa = {
  trans: (int * 'a * int) list;     (* set of transitions *)
  init: int;                        (* initial state *)
  final: int list                   (* final states *)
};;

(* m1 deterministic and complete *)
let m1 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2);(2,'1',2)];
  init = 0;
  final = [1] }
;;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

(* m3 deterministic and non-complete *)
let m3 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',1);(1,'1',2)];
  init = 0;
  final = [1;2] }
;;

(* Utility functions on sets *)

let mkset l = List.fold_left (fun l' x -> if List.mem x l' then l' else x::l') [] l;;

let dup l = List.length l <> List.length (mkset l);;

let rec union l1 l2 = List.fold_left (fun l x -> if List.mem x l then l else x::l) l1 l2;;

let subseteq l l' = List.fold_right (fun x y -> if List.mem x l' then y else false) l true;;

let equals l l' = subseteq l l' && subseteq l' l;;

(* set of labels of a dfa *)
let getlabels m = 
  mkset (List.map (fun (q,a,q') -> a) m.trans)
;;

getlabels m1;;

(* set of labels in outgoing transition from state q *)
let outlabels m q =
  mkset (List.map (fun (q',a,q'') -> a)
           (List.filter (fun (q',a,q'') -> q'=q) m.trans))
;;

outlabels m2 2;;

(* set of states of a dfa *)
let getstates m = 
  mkset (List.flatten (List.map (fun (q,a,q') -> [q;q']) m.trans))
;;

getstates m1;;

(* determines if a dfa is complete *)
let is_complete m = 
  let qQ = getstates m in
  let lL = getlabels m in
  List.for_all (fun q -> subseteq lL (outlabels m q)) qQ
;;

assert (is_complete m1);;
assert (is_complete m2 = false);;
assert (is_complete m3 = false);;

(* pre: m.trans does not contain duplicates *)
let rec is_deterministic m = 
  not (dup (List.map (fun (q,a,q') -> (q,a)) m.trans))
;;

assert (is_deterministic m1);;
assert (is_deterministic m2 = false);;
assert (is_deterministic m3);;

(* pre: tl is deterministic and complete *)
let rec step q a = function
    [] -> failwith "DFA is not complete"
  | (q',b,q'')::l -> if q'=q && b=a then q'' else step q a l
;;

step 2 '0' m1.trans;;

(* pre: tl is deterministic and complete *)
let rec step_star q w tl = match w with
    [] -> q
  | a::w' -> step_star (step q a tl) w' tl
;;

step_star 0 ['0';'1';'1'] m1.trans;;


(* pre: m is deterministic and complete *)
let accept w m = List.mem (step_star m.init w m.trans) m.final;;

assert (accept ['0';'0';'1'] m1);;
assert (accept ['0';'0';'1';'1'] m1 = false);;
assert (accept ['1';'0';'0';'1'] m1 = false);;

let complete m sink =
  let qQ = getstates m in
  let lL = getlabels m in
  let tl = List.fold_left (fun tl q -> tl @ (List.map (fun a -> (q,a,sink)) lL)) [] qQ in
  let sl = List.filter (fun (q,a,_) -> not (List.mem a (outlabels m q))) tl
  in { trans = m.trans @ sl; init = m.init; final = m.final }
;;

let m3' = complete m3 3;;

assert (accept ['0';'1';'0';'1'] m3');;
assert (accept ['0';'0';'1';'0';'0'] m3');;
assert (accept ['0';'1';'1';'0'] m3' = false);;
