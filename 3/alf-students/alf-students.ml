type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;


(* id_of_noshow : student list -> string list *)
(* matriculation (id) of students who have not tried the exam *)
let id_of_noshow l = l
           |> List.filter (fun s -> s.vote=None)
           |> List.map (fun s -> s.id);;

id_of_noshow alf2023;;

(* upgradeable : student list -> string list *)
(* name surname of students who have a vote between 15 and 17 *)

let upgradeable l = l
           |> List.filter (fun s -> match s.vote with Some v when v>=15 && v<=17 -> true | _ -> false)
           |> List.map (fun s -> s.name ^ " " ^ s.surname)
;;
upgradeable alf2023;;

(* upgrade : student list -> student list *)
(* upgrades to 18 the votes from 15 to 17 *)

let upgrade l = l
           |> List.map (fun s -> match s.vote with Some v when v>=15 && v<=18 -> { s with vote = Some 18 } | _ -> s)
;;
upgrade alf2023;;

(* wrong_laude : student list -> string list *)
(* name surname of students who have a laude but no vote or vote less than 30 *)
let wrong_laude l = l
           |> List.filter (fun s -> (match s.vote with Some 30 -> false | _ -> true) && s.laude)
           |> List.map (fun s -> s.name ^ " " ^ s.surname)
;;
wrong_laude alf2023;;

(* fix_laude : student list -> student list *)
(* sets to false the laude if the student has not given the exam or the vote is less than 30 *)

let fix_laude l = l
           |> List.map (fun s -> if (match s.vote with Some 30 -> false | _ -> true) && s.laude then { s with laude=false } else s)
;;
fix_laude alf2023;;

(* percent_passed : student list -> int *)
(* percentage of students who have passed the exam (vote >= 18) *)

let percent_passed l = l
           |> List.filter (fun s -> (match s.vote with Some x when x>=18 -> true | _ -> false))
           |> List.length
           |> fun x -> (x * 100)/(List.length l)
;;
percent_passed alf2023;;

(* avg_vote : student list -> float *)
(* average vote of students who have passed the exam (vote >= 18); the laude adds 2 points *)

let avg_vote l =  l
            |> List.filter (fun s -> (match s.vote with Some x when x>=18 -> true | _ -> false))
            |> List.map (fun s -> match s.vote with Some v -> v + (if s.laude then 2 else 0) | _ -> failwith "abort")
            |> fun l' -> (List.fold_left (+) 0 l', List.length l')
            |> fun (x,n) -> (float_of_int x) /. (float_of_int n);;

avg_vote alf2023;;
