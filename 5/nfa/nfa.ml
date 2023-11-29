(**********************************************************************
 * DFAs 
   States and labels are polymorphic
   pre: trans has no duplicates
   pre: init in states(trans)
   pre: for all q in final . q in states(trans)
 **********************************************************************)

type ('a,'b) fsa = {
  trans: ('a * 'b * 'a) list;       (* set of transitions *)
  init: 'a;                         (* initial state *)
  final: 'a list                    (* final states *)
};;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;


(* Utility functions on sets *)

let mkset l = List.fold_left (fun l' x -> if List.mem x l' then l' else x::l') [] l;;

let dup l = List.length l <> List.length (mkset l);;

let union l1 l2 = List.fold_left (fun l x -> if List.mem x l then l else x::l) l1 l2;;

let inter xl yl = List.fold_left (fun il y -> if List.mem y xl then y::il else il) [] yl;;

let diff xl yl = List.fold_left (fun dl x -> if List.mem x yl then dl else x::dl) [] xl;;

let subseteq l l' = List.fold_right (fun x y -> if List.mem x l' then y else false) l true;;

let equals l l' = subseteq l l' && subseteq l' l;;


(**********************************************************************
 * getlabels : 'a fsa -> 'a list
 **********************************************************************)

let getlabels m = 
  mkset (List.map (fun (q,a,q') -> a) m.trans)
;;


(**********************************************************************
 * subset_construction : 'a fsa -> 'a fsa
 **********************************************************************)

let fat_step1 ql a m =
  List.filter (fun (q,b,q') -> a=b && List.mem q ql) m.trans
  |> List.map (fun (q,b,q') -> q')
  |> mkset;;

assert (equals (fat_step1 [0] '0' m2) [1;0]);;
assert (equals (fat_step1 [0] '1' m2) []);;
assert (equals (fat_step1 [0;1] '0' m2) [0;1;2]);;
assert (equals (fat_step1 [0;1] '1' m2) [2]);;
assert (equals (fat_step1 [2] '0' m2) [2]);;
assert (equals (fat_step1 [2] '1' m2) []);;



let fat_step1_outtrans ql m = List.map (fun a -> (ql,a,fat_step1 ql a m)) (getlabels m);;

fat_step1_outtrans [0] m2;;

let fat_step1_outtrans_set qll m = 
  List.flatten (List.map (fun ql -> fat_step1_outtrans ql m) qll);;

fat_step1_outtrans_set [[0];[1]] m2;;

let fat_iter (trans,states) m =
  let trans' = fat_step1_outtrans_set states m in
  let states' = (List.map (fun (q,a,q') -> q') trans') in
  (union trans trans', mkset (union states states'))
;;

let (t21,s21) = fat_iter ([],[[m2.init]]) m2;;
let (t22,s22) = fat_iter (t21,s21) m2;;
let (t23,s23) = fat_iter (t22,s22) m2;;

let rec fix_fat_iter (trans,states) m =
  let (trans',states') = fat_iter (trans,states) m in
  if List.length trans' = List.length trans then trans'
  else fix_fat_iter (trans',states') m;;

let t2 = fix_fat_iter ([],[[m2.init]]) m2;;
mkset (List.map (fun (q,a,a') -> q) t2);;
t2;;

let states_of_trans tl = mkset (List.map (fun (a,b,c) -> a) tl);;
states_of_trans t2;;

let f2 = List.filter (fun q -> (inter q m2.final) <> []) (states_of_trans t2);;

{ trans = t2; init=[m2.init]; final = f2 };;

let subset_construction m =
  let trans' = fix_fat_iter ([],[[m.init]]) m in
  let init' = [m.init] in
  let final' = List.filter (fun q -> (inter q m.final) <> []) (states_of_trans trans') in
  { trans = trans'; init = init'; final = final' }
;;

let m2' = subset_construction m2;;

assert(accept ['0'] m2');;
assert(accept ['0';'0'] m2');;
assert(accept ['0';'1';'0'] m2' = false);;
