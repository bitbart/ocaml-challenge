(* f l = (ep,es) where 
   ep is the length of the longest even prefix of l 
   es is the length of the longest even subsequence of l
*)

let rec f = function
    [] -> (0,0)
  | a::l when a mod 2 = 0 ->
    let (ep,es) = f l in (ep+1,max (ep+1) es)
  | a::l -> let (ep,es) = f l in (0,es)
;;

let consecutive_even l = snd (f l);;

assert(consecutive_even [] = 0);;

assert(consecutive_even [1;2;3;4;5;6] = 1);; 

assert(consecutive_even [1;2;2;3;4;5] = 2);;

assert(consecutive_even [1;2;3;4;2;5] = 2);;

assert(consecutive_even [1;2;2;3;4;2;5] = 2);;

assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;

Random.self_init ();;

let rec rndeven p =
  let n = Random.int(50) in
  let b = Random.int(100) in
  if b<p then n*2 else n*2 + 1;;

(* p is the probability of even (0-100) *)
let rec rndlist n p =
  if n=0 then []
  else (rndeven p) :: (rndlist (n-1) p)
;;

(* 1: true *)
assert(consecutive_even [71; 92; 92; 84; 42; 14; 20; 46; 32; 16; 0; 52; 53; 84; 83; 36; 86; 36; 52; 0; 16; 78; 56; 66; 5; 30; 88; 13; 44; 57; 18; 23; 37; 54; 68; 63; 64; 74; 74; 37; 95; 72; 44; 52; 90; 34; 50; 98; 64; 60; 36; 70; 25; 84; 34; 90; 38; 15; 12; 12; 71; 32; 38; 26; 48; 44; 62; 54; 74; 62; 14; 8; 44; 98; 18; 20; 12; 88; 40; 4; 76; 78; 16; 36; 52; 41; 31; 1; 57; 98; 72; 2; 24; 80; 62; 58; 94; 67; 55; 10] = 24);;

(* 2: false *)
assert (consecutive_even [55; 4; 36; 34; 46; 23; 54; 45; 51; 68; 24; 37; 66; 58; 7; 92; 58; 15; 68; 18; 66; 86; 8; 78; 71; 40; 86; 44; 22; 60; 17; 52; 99; 40; 70; 80; 15; 82; 60; 44; 78; 68; 90; 83; 80; 15; 50; 38; 30; 80; 12; 56; 69; 56; 22; 71; 10; 24; 96; 76; 8; 80; 64; 25; 38; 96; 38; 43; 42; 70; 0; 87; 3; 25; 94; 62; 68; 78; 19; 3; 51; 44; 68; 54; 49; 68; 23; 58; 16; 44; 84; 99; 32; 0; 28; 54; 62; 58; 44; 54] = 7);;

(* 3: true *)
assert (consecutive_even [19; 50; 66; 93; 98; 54; 63; 86; 83; 58; 14; 12; 82; 34; 63; 2; 64; 54; 49; 83; 18; 38; 37; 13; 96; 70; 96; 34; 90; 89; 11; 40; 62; 72; 46; 65; 10; 48; 36; 50; 60; 69; 8; 30; 32; 4; 56; 76; 87; 97; 23; 72; 6; 22; 12; 93; 5; 86; 33; 58; 77; 13; 56; 30; 85; 50; 94; 8; 78; 87; 22; 84; 52; 75; 22; 84; 72; 81; 79; 72; 84; 26; 50; 64; 38; 21; 31; 4; 84; 50; 14; 66; 44; 20; 40; 97; 78; 34; 90; 72] = 8);;

(* 4: true *)
assert(consecutive_even [71; 83; 44; 86; 20; 56; 37; 68; 26; 16; 72; 59; 56; 48; 7; 18; 50; 61; 72; 20; 65; 21; 1; 64; 61; 72; 83; 64; 8; 18; 66; 84; 56; 62; 16; 63; 74; 49; 6; 86; 34; 2; 80; 37; 70; 55; 18; 60; 12; 74; 80; 52; 43; 14; 66; 10; 62; 68; 22; 93; 58; 20; 2; 1; 87; 72; 5; 80; 55; 50; 62; 38; 12; 98; 5; 20; 33; 65; 58; 82; 86; 5; 40; 42; 54; 14; 70; 77; 87; 74; 39; 78; 18; 66; 14; 50; 84; 88; 18; 10] = 9);;

(* 5: false *)
assert(consecutive_even [70; 56; 40; 97; 65; 99; 39; 84; 14; 40; 30; 98; 14; 62; 86; 56; 28; 76; 50; 30; 40; 44; 15; 46; 63; 96; 46; 72; 4; 46; 48; 53; 68; 28; 97; 40; 72; 48; 52; 18; 94; 98; 94; 86; 58; 10; 52; 59; 98; 13; 89; 42; 76; 42; 76; 44; 66; 71; 50; 82; 78; 76; 48; 96; 86; 31; 38; 46; 92; 72; 61; 87; 77; 60; 42; 39; 50; 32; 32; 45; 32; 28; 14; 23; 90; 0; 72; 48; 65; 65; 16; 55; 34; 0; 17; 46; 88; 2; 2; 36] = 14);;

(* 6: false *)
assert(consecutive_even [36; 28; 54; 36; 46; 20; 82; 25; 61; 55; 44; 32; 34; 93; 11; 98; 38; 46; 2; 4; 54; 50; 18; 64; 34; 48; 68; 46; 60; 64; 48; 79; 10; 2; 0; 2; 56; 7; 4; 98; 6; 50; 88; 72; 23; 49; 28; 37; 38; 12; 64; 70; 40; 62; 48; 72; 52; 40; 30; 23; 16; 16; 21; 18; 16; 13; 56; 90; 30; 30; 18; 58; 86; 39; 40; 76; 60; 30; 82; 51; 8; 20; 77; 22; 71; 98; 90; 38; 76; 23; 56; 89; 34; 65; 13; 2; 18; 89; 4; 70] = 15);;

let l3 = rndlist 100 80;;
