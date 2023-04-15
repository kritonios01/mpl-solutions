fun member (_,nil) = false
|	member (x,a::t) = if x=a then true else member(x,t)

fun less (_,nil) = nil
|	less (x,a::t) = if a<x then a::less (x,t) else less (x,t)

fun repeats nil = false
|	repeats [_] = false
|	repeats (a::b::t) = if a=b then true else repeats (b::t)

fun evaluate (nil, _) = 0.0
|	evaluate (l,x) = 
	let
		fun helper nil _ _ = 0.0
	 	|	helper (h::t) x i = 
			let
				fun pow (x,0) = 1.0
				|	pow (x,i) = x * pow(x,i-1)
			in
	 			h*pow(x,i) + helper t x (i+1)
	 		end	
	in
	 	helper l x 0
	end


fun quicksort nil = nil
|	quicksort (h::t) = 
	let
		fun less _ nil = nil
		|	less x (a::t) = if a<x then a::less x t else less x t

		fun more x nil = nil
		|	more x (h::t) = if h>=x then h::more x t else more x t

		val (l,r) = (quicksort (less h t), quicksort (more h t))
	in
		l @ [h] @ r
	end

fun quicksort2 (nil, _) = nil
|	quicksort2 (h::t, f) = 
	let
		fun less _ nil = nil
		|	less x (a::t) = if f (a, x) then a::less x t else less x t

		fun more x nil = nil
		|	more x (h::t) = if not (f (h, x)) then h::more x t else more x t

		val (l,r) = (quicksort2 ((less h t), f), quicksort2 ((more h t), f))
	in
		l @ [h] @ r
	end

(* functions to test quicksort2 *)
fun lcmp (a, b) = a < b ;
fun rcmp (a:real, b) = a < b ;
fun ircmp (a, b) = a > b

(* O(n^2) time complexity *)
fun intersection (_, nil) = nil
|	intersection (nil, _) = nil
|	intersection (h::t,l) = if member(h,l) then h::intersection(t,l) else intersection(t,l)