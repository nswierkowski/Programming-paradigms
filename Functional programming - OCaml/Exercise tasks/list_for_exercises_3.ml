(*Zadanie2*)
let curry3 f x y z = f (x,y,z);;
let curry32 = function f -> function x -> function y -> function z -> f(x,y,z);;
let uncurry3 f (x,y,z) = f x y z;;
let uncurry32 = function f -> function (x,y,z) -> f x y z

(*Zadanie3*)
let sumProd xs =
  let sumProdIn x y =
    let (sum, product) = x in
    (sum + y, product * y)
  in
  List.fold_left sumProdIn (0,1) xs
;;

(*Zadanie 5*)
let insertsort xs= 
match xs with
| [] -> []
| [x] -> [x]
| xs ->
  let insert x y = 
    let rec insertIn lastHead x y = 
    match x with
    | [] -> lastHead@x@[y]
    | h::t ->
      if h >= y then lastHead@[y]@x
      else insertIn (lastHead@[h]) t y
    in insertIn [] x y
  in List.fold_left insert [] xs
;;

let rec insertLists list1 list2 = 
    match list2 with 
    | [] -> list1
    | h2::t2 -> 
      match list1 with 
      | [] -> list2
      | h1::t1 -> 
        if h1 >= h2 then [h2]@(insertLists list1 t2)
        else [h1]@(insertLists t1 list2)
;;


let mergesort list = 
  let rec split list newList =
    match list with
    | [] -> newList
    | h::t -> split t newList@[[h]]
  in
  let rec mergeAll (list : int list list) (sortList : int list list) =
    let rec merge (left : int list) (right : int list) (tmp : int  list) =
      match right with
      | [] -> tmp@left
      | hr::tr ->
        match left with
        | [] -> tmp@right
        | hl::tl -> 
          if hl >= hr then merge left tr (tmp@[hr])
          else merge tl right (tmp@[hl])
    in
  match list with
  | [] -> sortList
  | h::t -> 
    match t with
    | [] -> sortList@[h]
    | h2::t2 -> mergeAll t2 (sortList@[merge (h) (h2) []])
  in
  let rec sort (splList : int list list) =
    let listOfList = mergeAll splList [] in
    match listOfList with
    | [] -> []
    | [x] -> x
    | _ -> sort(listOfList)
  in
  let splitedList = split list [] in
  sort (splitedList)
;;

