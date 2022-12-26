(*Zadanie 0*)
let log = fun prefix -> fun datetime -> fun text ->
  print_string ("[{"^prefix^"}] {"^datetime^"} \t {"^text^"} \n")
;;

(*Zadanie 1*)
let map list f = 
  let rec mapIn list newList f =
    match list with
    | [] -> newList
    | h::t -> mapIn t (newList@[f h]) f 
  in mapIn list [] f
;;

(*Zadanie 2 *)
let  filter list f = 
  let rec filterIn list newList f =
    match list with
      | [] -> newList
      | h::t -> 
       match f h with
       | true -> filterIn t (newList@[h]) f
       | false -> filterIn t newList f
  in filterIn list [] f
;;

(*Zadanie 3*)
let rec reduce list acc f =
  match list with
  | [] -> acc
  | h::t -> reduce t (f acc h) f
;;

(*Zadanie 4*)
let mean list = 
  (List.fold_left (fun x y -> x +. y) 0. list) /. (List.fold_left (fun x y -> x +. 1.0) 0. list)
;;

(*Zadanie 5*)
let toList str =
  let rec toListIn str list i length =
    if i >= length then list
    else toListIn str (list@[str.[i]]) (i+1) length
  in toListIn str [] 0 (String.length str)  
;;

  let toString list =
    let s = "" in
    let rec toStringIn str list =
      match list with
      | [] -> str
      | h::t -> toStringIn (String.concat str [""; String.make 1 h]) t
    in toStringIn s list
;;

let akronym str =
  let rec newReduce list acc f wasSpace=
    match list with
    | [] -> acc
    | h::t -> 
      match h with
      | ' ' -> newReduce t (acc) (f) (true)
      | _ -> 
        if wasSpace then newReduce t (f acc h) (f) (false)
        else newReduce t (acc) (f) wasSpace
  in
  let addFirstLetter acc h = String.concat acc [""; String.make 1 h] in
  newReduce (toList str) ("") (addFirstLetter) (true)
;;

(*This method is an alternative way of solving Task 5 
but is based on mutable variables and uses more variables than the above solution.*)
let alternativeAkronym str =
  let space = ref 0  (*Variable told as if last char was space (then its 0) else its 1*) in
  let wasSpace = ref 0 (*Variable will be only used to compare with variable space*) in
  let isThisFirstLetterOfWord c =
    let isThisFirstLetterOfWordIn c =
      match c with
    | ' ' -> space := 0; false
    | _ -> if space = wasSpace then space := 1; true
    in
    match c with
    | ' ' -> isThisFirstLetterOfWordIn c
    | _ -> 
      if space = wasSpace then isThisFirstLetterOfWordIn c
      else false
  in 
  toString (filter (toList str) isThisFirstLetterOfWord)
;;



(*Zadanie 6*)
let sqrListSmallerThenSum list =
  let sum = List.fold_left (fun x y -> x+.y) 0.0 list in 
  let isCubeSmallerThenSum x = if x*.x*.x <= sum then true else false in
  (List.filter (isCubeSmallerThenSum) list )
;;  