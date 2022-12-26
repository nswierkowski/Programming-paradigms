type point2D = float * float;;

let distance first second =
  match first with
  | (a, b) -> 
    match second with
    | (c, d) -> sqrt((a -. c)*.(a -. c) +. (b -. d)*.(b -. d))
;;

type point3D = float * float * float 

let distance3D first second =
  match first with
  | (a, b, c) -> 
    match second with
    | (d, e, f) -> sqrt((a -. d)*.(a -. d) +. (b -. e)*.(b -. e) +. (c -. f)*.(c -. f)) 
;;


type 'a listN = ('a)
let (xn : 'float listN) = [1.;1.;1.;1.;1.;1.];;

let distanceN newListN1 newListN2=
  let rec distanceNIn sum newListN1 newListN2 =
    match newListN1 with
    | [] -> sqrt(sum)
    | h1::t1 ->
      match newListN2 with
      | [] -> sqrt(sum)
      | h2::t2 -> distanceNIn (sum +. (h1 -. h2)*.(h1 -. h2)) (t1) (t2)
    in
    match newListN1 with
    | (list1) ->
      match newListN2 with
      | (list2) -> distanceNIn 0.0 list1 list2
;; 

(*Zadanie 2*)
(*Krotkowe rozwiązanie*)
type person = string * string * int * bool * int;;
type partnership = person * person;;

let (first : person) = "Jan", "Kowalski", 45, true, 42;;
let (second : person) = "Anna", "Kowalski", 46, false, 38;;

let (couple : partnership) = first, second;;

let giveMeYounger partners =
  match partners with 
  | (a, b) -> 
    match a with 
    | (aName, aSurname, aAge, aSex, aShoesNumber) ->
      match b with 
      | (bName, bSurname, bAge, bSex, bShoesNumber) ->
        if aAge > bAge then b
        else a
;;


(*Record rozwiązanie*)
type newPerson = {
    name : string;
    surname : string;
    age : int;
    sex : bool;
    shoesNumber : int;
  };;

type newPartnership = {
  first : newPerson;
  second : newPerson;
};;

let (jN : newPerson) = {name = "Jan"; surname = "Nowak"; age = 45; sex = true; shoesNumber = 42};;
let (aN : newPerson) = {name = "Anna"; surname = "Nowak"; age = 46; sex = false; shoesNumber = 38};;
let (couple : newPartnership) = {first = jN; second = aN};;

let newGiveMeYounger {first = a; second = b} =
  match a with 
   | {name = aName; surname = aSurname; age = aAge; sex = aSex; shoesNumber = aShoesNumber} ->
     match b with 
     | {name = bName; surname = bSurname; age = bAge; sex = bSex; shoesNumber = bShoesNumber} ->
       if aAge > bAge then b
       else a
;; 

(*Zadanie 3*)
type weekDay = Poniedzialek | Wtorek | Sroda | Czwartek | Piatek | Sobota | Niedziela;;

let weekDayToString day =
  match day with 
  | Poniedzialek -> "Poniedziałek"
  | Wtorek -> "Wtorek"
  | Sroda -> "Środa"
  | Czwartek -> "Czwartek"
  | Piatek -> "Piątek"
  | Sobota -> "Sobota"
  | Niedziela -> "Niedziela"
;;

let nextDay day =
  match day with 
  | Poniedzialek -> Wtorek
  | Wtorek -> Sroda
  | Sroda -> Czwartek
  | Czwartek -> Piatek
  | Piatek -> Sobota
  | Sobota -> Niedziela
  | Niedziela -> Poniedzialek
;;

(*Zadanie 4*)
type 'a maybe = Just of 'a | Nothing;;

let safeHead xs =
  match xs with
  | [] -> Nothing
  | h::t -> Just(h)
;;

(*Zadanie 5*)
type cuboid = float * float * float;;
type cone = float * float;;
type sphere = float;;
type cylinder = float * float;;

type solidFigue = Cuboid of cuboid | Cone of cone | Sphere of sphere | Cylinder of cylinder;;

let (p1 : cuboid) = 1.0, 1.0, 1.0;;
let (p2 : cone) = 1.0, 1.0;;
let (p3 : sphere) = 1.0;;
let (p4 : cylinder) = 1.0, 1.0;;
let (cuboid1 : solidFigue) = Cuboid p1;;
let (cone1 : solidFigue) = Cone p2;;
let (sphere1 : solidFigue) = Sphere p3;;
let (cylinder1 : solidFigue) = Cylinder p4;;

let volume figure =
  match figure with
  | Cuboid  (a, b, h)-> a*.b*.h
  | Cone (r, h)-> ((3.14 *. r *. r) *. h)/.3.0
  | Sphere (r) -> (4.0 *. 3.14 *. r *. r)/.3.0
  | Cylinder (r, h) -> (3.14 *. r *. r) *. h
;;