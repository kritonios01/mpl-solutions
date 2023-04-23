datatype cards = hearts | tiles | clovers | pikes

fun card_name x = 
	case x of
		  hearts => "hearts"
		| tiles => "tiles"
		| clovers => "clovers"
		| pikes => "pikes"

datatype number = 
	  INT of int 
	| REAL of real

fun plus (INT x) (INT y) = INT (x + y)
|	plus (REAL x) (REAL y) = REAL (x + y)
|	plus (REAL x) (INT y) = REAL (x + real y)
|	plus (INT x) (REAL y) = REAL (real x + y)

datatype int_nest =
	INT of int
|	LIST of int_nest list

fun addup (INT x) = x
|	addup (LIST nil) = 0
|	addup (LIST (h::t)) = addup h + addup (LIST t)

datatype 'element mylist = 
	NIL 
|	CONS of 'element * 'element mylist

fun product NIL = 1
|	product (CONS (h,t)) = h * product t

(* tail-recursive *)
fun reverse l =
	let
		fun helper NIL acc = acc
		|	helper (CONS (h,t)) acc = helper t (CONS (h,acc))
	in
		helper l NIL
	end

fun append l r = 
	let
		fun helper NIL b = b
		|	helper a NIL = a
		|	helper (CONS(h,t)) b = helper t (CONS(h, b))
	in
		helper (reverse l) r
	end

datatype 'a tree = Empty | Node of ('a tree) * 'a * ('a tree)

fun append_all Empty = nil
|	append_all (Node(l,n,r)) = (append_all l) @ n @ (append_all r)

fun complete_tree Empty = true
|	complete_tree (Node(l,_,r)) = 
		if (l<>Empty andalso r<>Empty) then (complete_tree l) andalso (complete_tree r) 
		else if (l=Empty andalso r=Empty) then true else false

(* THIS IS WRONG!!!
fun makeBST nil _ = Empty
|	makeBST [a] _ = Node(Empty,a,Empty)
|	makeBST (a::b::t) f = 
		if f(a,b) then Node(makeBST (a::t) f,b,Empty)
		else Node(Empty, a, makeBST (b::t) f)
*)

fun searchBST Empty _ = false
|	searchBST (Node(l,n,r)) x = 
		if n=x then true 
		else if x>n then searchBST r x
		else searchBST l x