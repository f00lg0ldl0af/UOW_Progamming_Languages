(* PRACTICE QUESTIONS *)

(* Q1 *)
fun alternate (xs : int list) =
    if null xs then 0
    else hd xs - alternate (tl xs)

(* Q2 *)
fun min_max (xs : int list) =
    if null (tl xs) then (hd xs , hd xs)
    else
	let
	    fun get_min (n1 : int , n2 : int) = if n1 < n2 then n1 else n2
	    fun get_max (n1 : int , n2 : int) = if n1 > n2 then n1 else n2
	    val x = hd xs
	    val tail = min_max (tl xs)
	in
	    (get_min (x , #1 tail) , get_max (x, #2 tail) )
	end

(* Q3 *)
fun cumsum (xs : int list) =
    if null xs then [] else     (* 0 element *)
    if null (tl xs) then [hd xs](* 1 element *)			
    else
	let
	    val tail = tl xs
	    val sum = hd xs + hd tail
	in
	    hd xs :: cumsum (sum :: tl tail)
	end

(* Q4 *)
fun greeting (name : string option)=
    let
	val input = if isSome name then valOf name else "you"
    in
	"Hello there, " ^ input ^ "!"
    end

	    
(* Q5 *)
fun repeat (xs : int list , count : int list) =
    let
	val reduce_count = (hd count - 1) :: tl count
    in
	if null xs then [] else
	if hd count = 0 then repeat (tl xs, tl count) else
	hd xs :: repeat(xs , reduce_count)
    end

	
(* Q6 *)
fun addOpt (o1 : int option , o2 : int option) =
    if isSome o1 andalso isSome o2
    then SOME (valOf o1 + valOf o2)
    else NONE

	     
(* Q7 *)
fun addAllOpt (lst : int option list) =
    if null lst then NONE else
    let	fun get_val (os : int option list) =
	if null os then 0 else              (* Base case: reach end of os*)
	if isSome (hd os)
	then valOf (hd os) + get_val (tl os)
        else get_val (tl os)					 
    in
	SOME (get_val lst)
    end 
	
(* Q8 *)
fun any (bs : bool list) =
    if null bs
    then false
    else hd bs orelse any (tl bs) (* andalso works for
				   if e1 then e2 else false *)
	
(* Q9 *)
fun all (bs : bool list) =
    null bs orelse hd bs andalso any (tl bs)

(* Q10 *)
fun zip (L1 : int list , L2 : int list) =
    if null L1 orelse null L2 then [] else
    (hd L1 , hd L2) :: zip(tl L1 , tl L2)

(* Q11 *)
(*
fun zipRecycle (data1 : int list, data2 : int list) =
    if null data1 orelse null data2 then [] else
    let
        fun helper (d1 : int list, d2 : int list, short_one : int) =
            let
                val data = (hd d1, hd d2);
                val is_empty_1 = null(tl d1);
                val is_empty_2 = null(tl d2);
                val is_all_empty = is_empty_1 andalso is_empty_2

		val is_list_1 = (short_one = 0 orelse short_one = 1);
                val is_list_2 = (short_one = 0 orelse short_one = 2);
                val is_repeat_1 = is_list_1 andalso is_empty_1;
                val is_repeat_2 = is_list_2 andalso is_empty_2;
		
                val is_end_1 = not is_list_1 andalso is_empty_1;
                val is_end_2 = not is_list_2 andalso is_empty_2;
                val is_ended = is_end_1 orelse is_end_2;
            in
                if is_all_empty orelse is_ended then [data] else
                if is_repeat_1 then data :: helper(data1, tl d2, 1) else
                if is_repeat_2 then data :: helper(tl d1, data2, 2) else
                data :: helper(tl d1, tl d2, short_one)
            end;
    in
        helper(data1, data2, 0)
    end;

*)
fun zipRecycle (L1 : int list , L2 : int list) =
    if null L1 orelse null L2 then [] else

    let fun helper (d1 : int list , d2 : int list , shorter : int) =
	    let
		val first = (hd d1 , hd d2)
		val empty_L1 = null (tl d1)
		val empty_L2 = null (tl d2)
		val both_empty = empty_L1 andalso empty_L2
		(* need indicator that longer list has finished, w/o which,
	    both lists recycles themselves for a while till coincidentally
	    we reach end of both lists at the same time *)
		val done_L1 = (shorter = 2) andalso empty_L1
		val done_L2 = (shorter = 1) andalso empty_L2
		val longer_done = done_L1 orelse done_L2
	    in 
		if both_empty orelse longer_done then [first] else
		if empty_L1 then first :: helper (L1 , tl d2 , 1) else
		if empty_L2 then first :: helper (tl d1 , L2 , 2) else
		first :: helper (tl d1 , tl d2 , shorter)
	    end
    in
	helper (L1 , L2 , 0)
    end
			  
(*			 
fun zipRecycle (L1 : int list , L2 : int list) =
    if null L1 orelse null L2 then [] else
    let
	val first = (hd L1 , hd L2)
	val empty_L1 = null (tl L1)
	val empty_L2 = null (tl L2)
			    
    in
	if empty_L1 andalso empty_L2 then [first] else
	if empty_L1 then first :: zipRecycle (L1 , tl L2) else
	(* last val-binding to L1 was [3] , hence such  *)
	if empty_L2 then first :: zipRecycle (tl L1 , L2) else
	first :: zipRecycle (tl L1 , tl L2)
	
    end

	
 results: idk why
- zipRecycle ([1,2,3], [1, 2, 3, 4, 5, 6, 7]);
val it = [(1,1),(2,2),(3,3),(3,4),(3,5),(3,6),(3,7)] : (int * int) list
*)

				     
		      

    
