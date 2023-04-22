fun cube x = x * x * x

fun cuber x:real = x * x * x

fun fourth x = hd (tl (tl (tl x)))

fun nth x 1 = hd x (* extra *)
|	nth x i = nth (tl x) (i-1)

fun min_of_3 (x,y,z) =
	if x <= y andalso y <= z then x
	else if y <= z andalso y <= z then y
	else z

fun cut_2nd (x,y,z) = (x,z)

fun third_char x = nth (explode x) 3 (* extra *)
fun third_char2 x = hd (tl (tl (explode x))) 

fun cycle1 x = (tl x) @ [hd x]

fun sort3 (x:real,y:real,z:real) =
	if x <= y then
		if y <= z then [x,y,z]
		else if x <= z then [x,z,y]
		else [z,x,y]
	else
		if y >= z then [z,y,x]
		else if x >= z then [y,x,z]
		else [y,z,x]

fun delete3 (a::b::c::t) = a::b::t
|	delete3 x = x (*this is for less than 3 items. It silences the match non exhaustive warning*)

fun square_sum 1 = 1
|	square_sum x = x*x + (square_sum (x-1))

(* tail-recursive version for optimized memory complexity *)
fun square_sum2 x = 
	let
		fun walk 0 acc = acc
		|	walk x acc = walk (x-1) (acc + x*x)
	in
		walk x 0
	end

(* time complexity O(n^2) !!! *)
fun cycle_over (x, 0) = x
|	cycle_over (x, i) = cycle_over (cycle1 x, i-1) 

fun power (x:real, 0) = 1.0
|	power (x, i) = x * power (x, i-1)

(* tail-recursive version *)
fun power2 (x:real, i) =
	let
		fun walk _ 0 _ = 1.0
		|	walk _ 1 acc = acc
		|   walk x i acc = walk x (i-1) acc*x
	in
		walk x i x
	end

fun max x =
	let
		fun walk nil acc = acc
		|	walk (h::t) acc = if h>acc then walk t h else walk t acc
	in
		walk (tl x) (hd x)
	end

fun isPrime 1 = false
|	isPrime x =
	let
		fun helper x 1 = true
		|	helper x d = if x mod d = 0 then false else helper x (d-1)
	in
		helper x (x div 2)
	end

(* not very smart AKA mesw lamias *)
fun select (x,f) = 
	let
		fun helper nil _ acc = acc
		|	helper (h::t) f acc = if f h = true then helper t f (h::acc) else helper t f acc
	in
		helper x f nil
	end

(* simpler version *)
fun select2 (nil, _) = nil
|	select2 (h::t,f) = if f h then h::select2 (t,f) else select (t,f)