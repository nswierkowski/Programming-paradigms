(*Zadanie1*)
let rec flatten(xss) = 
  if xss = [] then []
  else ((List.hd xss)@flatten(List.tl xss))
;;

(*Zadanie 2*)
let rec count(x, xs) =
  if xs = [] then 0
  else
    if (List.hd xs) = x then (1+count(x, (List.tl xs)))
    else (count(x, (List.tl xs)))
;;

(*Zadanie 3*)
let rec replicate(x, n) =
  if n = 0 then []
  else [x]@replicate(x, n-1)
;; 

(*Zadanie 4*)
let rec sqrList(xs) =
  if xs = [] then []
  else [(List.hd xs) *. (List.hd xs)]@sqrList((List.tl xs))
;;

(*Zadanie 5*)
let palindrome(xs) =
  let rec areTheseListTheSame(first, second) =
    if first = [] || second = [] then true
    else 
      if (List.hd first) = (List.hd second) then true && areTheseListTheSame((List.tl first), (List.tl second))
      else false
    in areTheseListTheSame(xs, (List.rev xs))     
;;

let palindrome2(xs) =
  if xs = List.rev xs then true
  else false  
;;

(*Zadanie 6*)
let rec listLength(xs) =
  if xs = [] then 0
  else 1+listLength((List.tl xs))
;;  

(*Zadanie 7*)   
let rec power(n) =
  if n = 1.0 then 0.0
  else 1.0+.power(n/.2.0)
;;

let newFunction(n, c) =
  if n = 1.0 then 1.0
  else
    let functionValue(k) =
    0.5*.(c*.k*.k +. c*.k+.2.0)
  in functionValue(power(n))
;;  
    