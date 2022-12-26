(*Zadanie 1*)
module type Point_3D =
sig
  type 'a point

  exception Wrong_type of string

  val create_int : int -> int -> int -> int point

  val create_float : float -> float -> float -> float point

  val create_other : 'a -> 'a -> 'a -> 'a point

  val distance : 'a point * 'a point -> float

end;;

module Point : Point_3D  =
struct

type 'a point = Point_int of int * int * int | Point_float of float * float * float | Point of 'a * 'a * 'a

exception Wrong_type of string

let create_int (a : int) (b : int) (c : int) = Point_int(a,b,c)

let create_float (a : float) (b : float) (c : float) = Point_float(a,b,c)

let create_other (a : 'a) (b : 'a) (c : 'a) = Point(a,b,c)

let distance ((p1 : 'a point), (p2 : 'a point)) = 
match (p1,p2) with
| (Point_int(a, b, c), Point_int(d,e,f)) -> sqrt (float_of_int ((a - d)*(a - d) +(b - e)*(b - e) +(c - f)*(c - f)))
| (Point_float(a,b,c), Point_float(d,e,f)) -> sqrt (((a -. d)*.(a -. d) +. (b -. e)*.(b -. e) +. (c -. f)*.(c -. f)))
| _ -> raise (Wrong_type "module Point: distance")

end;;

(*Zadanie2*)
module type Segments = 
sig
  type 'a segment = ('a Point.point) * ('a Point.point)

  val create : 'a Point.point -> 'a Point.point -> 'a segment

  val length : 'a segment -> float

end;;

module Segment : Segments =
struct
  type 'a segment = ('a Point.point) * ('a Point.point)

  let create (p1 : 'a Point.point) (p2 : 'a Point.point) = (p1,p2)
  
  let length (s : 'a segment) = 
    match s with
    | (p1,p2) -> Point.distance (p1,p2)

end;;

(*Zadanie 3*)
module type BST =
sig
  type 'a tree

  val empty : 'a tree

  val create : 'a -> 'a tree

  val add : 'a tree -> 'a -> 'a tree

  val delete : 'a tree -> 'a -> 'a tree

  val remove : int tree -> int -> int tree

  val pre_order : 'a tree -> 'a list

  val in_order : 'a tree -> 'a list

  val post_order : 'a tree -> 'a list

  val to_list : 'a tree -> 'a list

end;;

module BinaryTree : BST =
struct
  type 'a tree = Nil | Cons of 'a * 'a tree * 'a tree

  let empty = Nil

  let create (node : 'a) = Cons(node, Nil, Nil)

  let ex = Cons(5, Cons(3,Nil,Nil), Cons(8,Cons(6,Nil,Nil),Nil))

  let getValue (tree : 'a tree) =
    match tree with
    | Nil -> 0
    | Cons(v,l,r) -> v

  let rec add (t : 'a tree) (node : 'a) = 
    match t with
    Nil -> Cons(node, Nil, Nil)
    | Cons(v, (l : 'a tree), (r : 'a tree)) ->
      if v > node then 
        match l with
        Nil -> Cons(v, Cons(node, Nil, Nil),r)
        |Cons(_, _, _) -> Cons(v, l, add l node)
      else if v < node then 
        match r with
        Nil -> Cons(v, l,Cons(node, Nil, Nil))
        |Cons(_, _, _) -> Cons(v, l, add r node)
      else t

  let rec delete (t : 'a tree) (node : 'a) =
    match t with
    Nil -> t
    | Cons(v, (l : 'a tree), (r : 'a tree)) ->
      if v = node then Nil
      else if v < node then delete r node
      else delete l node
  
  let rec remove (t : 'a tree) (node : 'a) =
    let rec find_the_biggest (t : 'a tree) =
      match t with
      | Nil -> t
      | Cons(v,l,r) -> 
        match r with 
        | Nil -> r
        | Cons(_,_,_) -> find_the_biggest r
    in
    match t with
    Nil -> t
    | Cons(v, (l : 'a tree), (r : 'a tree)) ->
      if v = node then 
        let next = find_the_biggest t in
        match (l,r) with
        | (Nil, Nil) -> Nil
        | (Cons(_,_,_), Nil) -> l
        | (Nil, Cons(_,_,_)) -> r
        | (Cons(_,_,_),Cons(_,_,_)) -> Cons(getValue next, l, remove r (getValue next))
      else if v < node then Cons(v,l,delete r node)
      else Cons(v,l,delete l node)

  let pre_order (t : 'a tree) =
    let list_of_nodes = [] in
    let rec pre_order_in (t : 'a tree) =
      match t with
      Nil -> []
      | Cons(v, l, r) -> list_of_nodes@[v]@(pre_order_in l)@(pre_order_in r)
    in pre_order_in t
    
    let in_order (t : 'a tree) =
      let list_of_nodes = [] in
      let rec in_order_in (t : 'a tree) =
        match t with
        Nil -> []
        | Cons(v, l, r) -> list_of_nodes@(in_order_in l)@[v]@(in_order_in r)
      in in_order_in t

    let post_order (t : 'a tree) =
      let list_of_nodes = [] in
      let rec post_order_in (t : 'a tree) =
        match t with
        Nil -> []
        | Cons(v, l, r) -> list_of_nodes@(post_order_in l)@(post_order_in r)@[v]
      in post_order_in t

  let to_list (t : 'a tree) =
    let rec to_list_in (t : 'a tree) list queue =
      match t with 
      Nil -> 
        if queue = [] then list
        else to_list_in (List.hd queue) list (List.tl queue)
      | Cons(v,l,r) ->
        if queue = [] then to_list_in l (list@[v]) [r]
        else to_list_in (List.hd queue) (list@[v]) (List.tl queue)
    in to_list_in t [] []

end;;

(*Zadanie 4*)
module Make_Point (P : sig type t end) = struct
  type 'a point =  {x : P.t; y : P.t; z : P.t}
  let create a b c = {x = a; y = b; z = c}
end;;

module Make_Point2 (P : Point_3D) = struct
  type 'a point = 'a P.point
  let create_int x y z = P.create_int x y z
  let create_float x y z = P.create_float x y z
  let create_other x y z = P.create_other x y z

end;;

module Make_Segment (P : Point_3D) = struct
  type 'a segment = 'a Point.point * 'a Point.point
  let create (x : 'a Point.point) (y : 'a Point.point) = (x,y) 
end;;

module type Translate = sig
  open Point
  
  val translate_int : int Point.point -> int -> int -> int -> int Point.point 

  val translate_float : float Point.point -> float -> float -> float -> float Point.point 

end;;

module Translate_Point (P : Point_3D) (T : Translate) = struct

  let translate_int (p : int Point.point) (x : int) (y : int) (z : int) = T.translate_int p x y z

  let translate_float (p : float Point.point) (x : float) (y : float) (z : float) = T.translate_float p x y z
end;;

module Translate_Segment (S : Segments) (T : Translate) = struct

  let translate_int (s : int S.segment) (x : int) (y : int) (z : int) =
    match s with
    | (p1,p2) -> ((T.translate_int p1 x y z),(T.translate_int p2 x y z))

  let translate_float (s : float S.segment) (x : float) (y : float) (z : float) =
    match s with
    | (p1,p2) -> ((T.translate_float p1 x y z),(T.translate_float p2 x y z))
end;;
