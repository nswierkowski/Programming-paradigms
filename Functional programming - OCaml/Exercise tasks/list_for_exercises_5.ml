type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec toLazyList = function
[] -> LNil
| x :: xs -> LCons(x, lazy (toLazyList xs))
;;
let rec ltake = function
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs)
;;

(*Zadanie2*)
let lfib = 
    let rec lfibIn(p, n) =
        LCons(p+n, lazy(lfibIn(n, p+n))) in
    LCons(1, lazy(LCons(1, lazy(lfibIn(1, 1)))));;
    
ltake(15, lfib);;
 