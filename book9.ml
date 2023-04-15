fun int_to_real x = map real x

fun ord_list x = map ord x

fun square_list x = map (fn x => x * x) x

fun mult_pairs x = map op * x

fun inc_list l x = foldr (fn (a, b) => (a+x) :: b) [] l
fun inc_list2 l n = map (fn x => x + n) l

fun square_sum l = foldr (fn (a, b) => a*a + b) 0 l

fun list_or l = foldr (fn (a, b) => a orelse b) false l

fun list_and l = foldr (fn (a, b) => a andalso b) true l

fun list_xor l = foldr (fn (a, b) => if b = a then false else true) false l 

fun dup_list l = foldr (fn (a, b) => a::a::b) [] l

fun my_length l = foldr (fn (a, b) => b+1) 0 l

fun abs_values l = foldr (fn (a, b) => (if a>=0 then real a else real (~a))::b) [] l

fun true_count l = foldr (fn (a, b) => if a then b+1 else b) 0 l

fun max_pairs l = foldr (fn (a, b) => let val (x, y) = a in if x>y then x::b else y::b end) [] l
fun max_pairs2 l = map (fn (l,r) => if l>r then l else r) l

fun my_implode l = foldr (fn (a, b) => str(a)^b) "" l
fun my_implode2 l = foldl (fn (a, b) => b^str(a)) "" l

fun my_append l = foldr (fn (a, b) => a @ b) [] l

fun max l = foldr (fn (a, b) => if a>b then a else b) (hd l) l

fun min l = foldr (fn (a, b) => if a<b then a else b) (hd l) l

fun member (x:int,l) = foldr (fn (a, b) => if x=a then true else b orelse false) false l







fun my_map _ nil = nil
|	my_map f (h::t) = (f h)::my_map f t

fun my_foldl _ c nil = c
|	my_foldl f c (h::t) = my_foldl f (f (h,c)) t

fun my_foldr _ c nil = c
|	my_foldr f c (h::t) = f (h, my_foldr f c t)