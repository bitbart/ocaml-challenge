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

assert (equals (getlabels m1) ['0';'1']);;
assert (equals (getlabels m2) ['0';'1']);;
assert (equals (getlabels m3) ['0';'1']);;

(* set of labels in outgoing transition from state q *)
let outlabels m q =
  mkset (List.map (fun (q',a,q'') -> a)
           (List.filter (fun (q',a,q'') -> q'=q) m.trans))
;;

assert (equals (outlabels m1 0) ['0';'1']);;
assert (equals (outlabels m1 1) ['0';'1']);;
assert (equals (outlabels m1 2) ['0';'1']);;
assert (equals (outlabels m2 2) ['0']);;

(* set of states of a dfa *)
let getstates m = 
  mkset (List.flatten (List.map (fun (q,a,q') -> [q;q']) m.trans))
;;

assert (equals (getstates m1) [0;1;2]);;
assert (equals (getstates m2) [0;1;2]);;
assert (equals (getstates m3) [0;1;2]);;

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
let rec step1_rec q a = function
    [] -> failwith "DFA is not complete"
  | (q',b,q'')::l -> if q'=q && b=a then q'' else step1_rec q a l

let step1 q a m = step1_rec q a m.trans;;

assert (step1 0 '0' m1 = 0);;
assert (step1 0 '1' m1 = 1);;
assert (step1 1 '0' m1 = 2);;
assert (step1 1 '1' m1 = 2);;
assert (step1 2 '0' m1 = 2);;
assert (step1 2 '1' m1 = 2);;

(* pre: m is deterministic and complete *)
let rec step q w m = match w with
    [] -> q
  | a::w' -> step (step1 q a m) w' m
;;

assert(step 0 ['0';'0';'0'] m1 = 0);;
assert(step 0 ['0';'1';'1'] m1 = 2);;


(* pre: m is deterministic and complete *)
let accept w m = List.mem (step m.init w m) m.final;;

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
