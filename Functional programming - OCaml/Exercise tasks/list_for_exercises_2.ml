(*Zadanie 2*)
let rec fib(n) = 
  match n with
  | 0 -> 0 
  | 1 -> 1
  | n -> fib(n-1) + fib(n-2)
;;

let fibTail(n) =
 match n with
 | 0 -> 0
 | 1 -> 1
 | n ->
    let rec fibTailIn(n , prev, beforePrev) =
      if n = 1 then prev
      else fibTailIn(n-1, prev+beforePrev, prev)
    in fibTailIn(n, 1, 0)
;;   

(*Zadanie 3*)
let check (a) = 
  if a > 1.0 then a/.3.0
  else a
;;

let root3 (a, e) =
  if e <= 0.0 then 0.0
  else if a = 0.0 then 0.0
  else   
    let rec root3In(a, e, x) =
    let left = ((x*.x*.x -. a)/.a) in 
    if left > 0. && left <= e then x
    else if left < 0. && 0.-.left <= e then x
    else root3In(a, e, x +. ((a /. (x *. x)) -. x) /. 3.0)

  in root3In(a, e, check(a))
;;

(*Zadanie 5*)
let rec initSegment pref list =
  match pref with
  | [] -> true
  | h::t ->
    match list with
    | [] -> false
    | h2::t2 -> 
      if h = h2 then initSegment t t2
      else false
;;

(*Zadanie 6*)
let replaceNth list index newElement =
  let rec replaceNthIn list index newElement newList =
    if index < 0 then newList
    else 
    match list with 
    | [] -> list
    | h::t -> 
      if index = 0 then newList@[newElement]@t
      else replaceNthIn t (index-1) (newElement) (newList@[h])
    in replaceNthIn list index newElement []
;;

