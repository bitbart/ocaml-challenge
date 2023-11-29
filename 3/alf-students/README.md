# ALF students

Consider the following [record type](https://cs3110.github.io/textbook/chapters/data/records_tuples.html) to represent the votes of university students:
```ocaml
type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}
```

The votes of students attending the ALF course are stored in the following list:
```ocaml
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
```

Write the following functions.

```ocaml
id_of_noshow : student list -> string list
```
returns the matriculations (id) of students who have not tried the exam (i.e., whose vote is `None`).

```ocaml
upgradeable : student list -> string list
```
returns name/surname of students who have a vote between 15 and 17.

```ocaml
upgrade : student list -> student list
```
upgrades to 18 the votes from 15 to 17.

```ocaml
wrong_laude : student list -> string list
```
returns name/surname of students who have a laude but no vote or vote less than 30.

```ocaml
fix_laude : student list -> student list
```
sets to false the laude if the student has not given the exam or the vote is less than 30.

```ocaml
percent_passed : student list -> int
```
percentage of students who have passed the exam (vote >= 18).

```ocaml
avg_vote : student list -> float
```
average vote of students who have passed the exam (vote >= 18); the laude adds 2 points.


