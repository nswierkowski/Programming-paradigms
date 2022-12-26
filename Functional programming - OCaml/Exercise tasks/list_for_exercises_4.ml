let f1 x y z = x y z;;
let f2 x y = function z -> x::y

(*Zadanie2*)
let newF x = raise Not_found;;

let f x = let rec g x = g x in g x;;

(*Zadanie3*)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let rec nodes tree =
  match tree with
  Empty -> 0
  | Node(_,t1,t2) -> 1 + nodes t1 + nodes t2;;

let t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;

let tt = Node(1,
Node(2,
Node(4,
Empty,
Empty
),
Empty
),
Node(3,
Node(5,
Empty,
Node(6,
Empty,
Empty
)
),
Empty
)
);;

let  breadthBT tree =
  let addToQueue queue t1 =
    match t1 with
    | Empty -> queue
    | Node(a,b,c) -> queue@[t1]
  in
  let rec breadthBTIn tree list queue =
    match tree with
    | Empty -> list
    | Node(value, t1, t2) -> 
      let newQueue = addToQueue (addToQueue queue t1) t2 in
      match newQueue with 
      | [] -> list@[value]
      | h::t -> breadthBTIn h (list@[value]) t  
  in breadthBTIn tree [] []
;;

(*Zadanie 4*)
let internal tree =
  let rec wewIn tree sum =
    match tree with
    | Empty -> sum
    | Node(value, left, right) ->
      match (left, right) with
      | (Empty, Empty) -> sum
      | (Empty, Node(v2, l2, r2)) -> sum + wewIn right (sum+1)
      | (Node(v2, l2, r2), Empty) -> sum + wewIn left (sum+1)
      | (Node(vl, ll, rl), Node(vr, lr, rr)) -> sum +  wewIn right (sum+1) + wewIn left (sum+1)
  in wewIn tree 0
;;

let zew tree =
  let rec zewIn tree sum =
    match tree with
    | Empty -> sum
    | Node(value, left, right) -> (zewIn left (sum+1)) + (zewIn right (sum+1))
  in zewIn tree 0
;;

(*Zadanie 5*)
type 'a graph = Graph of ('a -> 'a list);;

let g = Graph
(function
0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;

let rec print_stack stack =
  match stack with 
  | [] -> print_endline
  | h::t -> print_int h;
    print_stack t
;;

let depthSearch2 (Graph g) start =
  let rec checkWhatsNextElement newElement stack =
    match g newElement with
    | [] -> checkWhatsNextStack newElement stack
    | h::t -> (g newElement)@stack
  and checkWhatsNextStack  newElement stack =
    match stack with
    | [] -> []
    | h::t -> stack
  in
  let rec depthSearch2In (newElement) visited stack =
    if List.mem newElement visited then 
      match stack with
      | [] -> visited
      | h::t -> depthSearch2In h visited t
    else
      let newStack = checkWhatsNextElement newElement stack in
      match newStack with
      | [] -> visited@[newElement]
      | h::t -> depthSearch2In h (visited@[newElement]) newStack
  in depthSearch2In start [] []
;;