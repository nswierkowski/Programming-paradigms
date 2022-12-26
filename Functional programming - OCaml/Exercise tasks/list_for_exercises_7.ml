module type QUEUE_FUN =
sig
 type 'a t
 exception Empty of string
 val empty: unit -> 'a t
 val enqueue: 'a * 'a t -> 'a t
 val dequeue: 'a t -> 'a t
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
end;;

(*ZADANIE 1A*)
module Queue_list : QUEUE_FUN =
struct
type 'a t = 'a list
exception Empty of string

let empty() = []

let enqueue (e,q) = q@[e]

let dequeue q = 
  match q with
  h::t -> t
  | [] -> []

let first q = 
  match q with
  |h::t -> h
  | _ -> raise (Empty "module Queue_list: first")

let isEmpty q = 
  match q with
  h::_ -> false
  | [] -> true
end;;

(*ZADANIE 1B*)
module Queue_two_lists : QUEUE_FUN =
struct
type 'a t = 'a list * 'a list
exception Empty of string

let empty() = ([],[])

let enqueue (e,(q : 'a t)) = 
  match q with
  ([],[]) -> (e::[],[])
  |([],y) -> (List.rev (e::y), [])
  | (x,y) -> (x, e::y)

let dequeue (q : 'a t) = 
  match q with 
  ([],[]) -> ([],[])
  | ([], y) -> (List.tl (List.rev y), [])
  | (x,y) -> (List.tl x, y)


let first (q : 'a t) = 
  match q with 
  ([],[]) -> raise (Empty "module Queue_two_lists: first")
  | ([], y) -> List.hd (List.rev y)
  | (x, y) -> List.hd x

let isEmpty (q : 'a t) = 
  match q with 
  ([],[]) -> true
  | _ -> false
end;;

(*ZADANIE 2*)
module type QUEUE_MUT =
sig
type 'a t
(* The type of queues containing elements of type ['a]. *)
exception Empty of string
(* Raised when [first q] is applied to an empty queue [q]. *)
exception Full of string
(* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
val empty: int -> 'a t
(* [empty n] returns a new queue of length [n], initially empty. *)
val enqueue: 'a * 'a t -> unit
(* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
val dequeue: 'a t -> unit
(* [dequeue q] removes the first element in queue [q] *)
val first: 'a t -> 'a
(* [first q] returns the first element in queue [q] without removing
it from the queue, or raises [Empty] if the queue is empty. *)
val isEmpty: 'a t -> bool
(* [isEmpty q] returns [true] if queue [q] is empty,
otherwise returns [false]. *)
val isFull: 'a t -> bool
(* [isFull q] returns [true] if queue [q] is full,
otherwise returns [false]. *)
end;;

module Queue_cyclic : QUEUE_MUT=
struct 
  type 'a t = {mutable r : int; mutable f : int; mutable length : int; mutable queue : 'a option array}
  exception Empty of string
  exception Full of string

  let empty (size : int) = {r = 0; f = 0; length = size; queue = Array.make size None}

  let enqueue (e,(q : 'a t)) = 
    if q.queue.(q.r) != None then raise (Full "module Queue_cyclic - enqueue")
    else
      q.queue.(q.r) <- Some e;
      if q.r + 1 < q.length then q.r <- q.r + 1
      else q.r <- 0

  let dequeue (q : 'a t) =
    q.queue.(q.f) <- None;
    if q.f + 1 < q.length then q.f <- q.f + 1
    else q.f <- 0

  let first (q : 'a t) = 
    match q.queue.(q.f) with
    Some elem -> elem
    |None -> raise (Empty "module Queue_cyclic - first")

  let isEmpty (q : 'a t) =
    match q.queue.(q.f) with
    Some elem -> false
    | None -> true
  
  let isFull (q : 'a t) =
    match q.queue.(q.r) with
    Some elem -> true
    | None -> false
end;;