(* 1. is_older evalutes true if d1 comes before d2.
   If d1 = d2, evaluates to false *)

fun is_older1 ((y1,m1,d1),(y2,m2,d2)) =
    let
	val year_equal = (y1 = y2)
	val year_less = (y1 < y2)
	val mth_equal  = (m1 = m2)
	val mth_less = (m1 < m2)
	val date_less = (d1 < d2)
    in			  
	if year_equal
	then
	    mth_equal andalso date_less orelse mth_less	    
	else year_less
    end
		      
	
		
fun is_older (d1 : int*int*int , d2 : int*int*int) =
    let
	val year_equal = (#1 d1) = (#1 d2)
	val year_less = (#1 d1) < (#1 d2)
	val mth_equal = (#2 d1) = (#2 d2)
	val date_less = (#3 d1) =(#3 d2)
	val mth_less = (#2 d1) < (#2 d2)
				     

    in
	if year_equal
	then
	    mth_equal andalso date_less orelse mth_less	    
	else year_less
    end

	


	
	  
(* Verbose solution :( BAD

fun is_older (d1 : int*int*int , d2 : int*int*int) =
    if (#1 d1) <= (#1 d2) 
    then
	if (#1 d1 = #1 d2)
	then
	    (#2 d1) = (#2 d2) andalso (#3 d1) < (#3 d2) orelse
	    (#2 d1) < (#2 d2)
	else
	    true (*d1 year < d2 year*)
    else false
 *)
	     
(* 2. takes a list of dates and a month, returns how many dates in the list are in given month *)

fun number_in_month (ds : (int*int*int) list , given_month : int) =
    if null ds
    then 0
    else
	let (* unnecessary fun get_months *)
	    fun get_months (ds : (int*int*int) list) =
		if null ds
		then []
		else #2 (hd ds) :: get_months(tl ds)
					   		    
	    fun check_month (ms: int list) =
		if null ms
		then 0
		else
		    if (hd ms) = given_month
		    then 1 + check_month (tl ms)
		    else check_month (tl ms)
	in
	    check_month (get_months ds)   
	end 
	    
(* 3. takes a list of dates and a list of months, returns number of dates
in the list of dates that are in any given months in list of month

ASSUME: no repeated number in list of months *)

fun number_in_months (ds : (int*int*int) list , ms : int list) =
    if null ms
    then 0
    else
	number_in_month (ds: (int*int*int) list, hd ms) +
	number_in_months (ds: (int*int*int) list , tl ms)
(* unnecessary inclusion of parameter type*)
    
    	    
(* 4. takes a list of dates and a month, returns a list holding dates from
argument list of dates that are in the month *)
			 
fun dates_in_month (ds : (int*int*int) list , month : int) =
    if null ds
    then []
    else
	if #2 (hd ds) = month
	then hd ds :: dates_in_month (tl ds , month : int)
	else dates_in_month (tl ds , month : int)
(* unnecessary inclusion of parameter type*)
			    
(* 5. takes a list of dates and a list of months, returns a list holding
the dates from the argument list of dates that are in list of months.
Use list-append @ operator

ASSUME: no repeated number in list of months
*)

fun dates_in_months (ds : (int*int*int) list , ms : int list) =
    if null ms
    then []
    else
	dates_in_month (ds : (int*int*int) list , hd ms) @
	dates_in_months (ds: (int*int*int) list , tl ms )

			
    
(* 6. takes a list of string and a int n, returns nth element of list
 where head of list is 1st *)

fun get_nth (xs : string list , n : int) =
    let	    
	val move_down_list = n - 1
				
	fun get_length_list (xs : string list) =
	    if null xs
	    then 0
	    else 1 + get_length_list (tl xs)

	val n_invalid = n <= 0 orelse n > get_length_list (xs : string list)
    
    in
	if null xs orelse n_invalid
	then ""
	else
	    if n = 1
	    then hd xs
	    else
		get_nth (tl xs, move_down_list)
    end 
	    
    
(* 7. takes a date eg. (2013, 1, 20),returns string of the form
January 20, 2013 *)

fun date_to_string (d : (int*int*int) ) =
    let
	val months = ["January" , "February" , "March" , "April",
		      "May" , "June" , "July" , "August" ,
		      "September" , "October" , "November" , "December"]
			 
	val get_month = get_nth (months , (#2 d))
		    
	val get_day  = Int.toString (#3 d)
			
	val get_year = Int.toString (#1 d)
	
    in
	get_month ^ " " ^ get_day ^ ", " ^ get_year 
	
    end 

(* 8. takes an int called sum and an int list, returns an int
such that first n elements of the list add to less than sum,
but the first (n + 1) elements of the list add to sum or more.

ASSUME:
- sum is positive
- list contains all positive numbers
- entire list sums to more than the passed in value
ok for an exception to occur if not the case  *)

fun number_before_reaching_sum (sum : int , original_xs : int list) =
    let
	fun get_lst_length (xs : int list) =
	    if null xs
	    then 0
	    else 1 + get_lst_length (tl xs)

	val lst_length = get_lst_length (original_xs)
					
	fun sum_list (xs : int list , count_num : int) =
	    if count_num = 0
	    then 0
	    else
		hd xs + sum_list (tl xs , count_num - 1)

	(* appreciate feedback  on minimising recomputation
	   for make_list_sums T_T
	 *)
		 
	fun make_list_sums (position : int) =
	    let
		val prev_sum = sum_list (original_xs , position) 
		val last_sum = sum_list (original_xs, lst_length)
	    in
		if position = lst_length		  
		then [last_sum]
		else
		    prev_sum :: make_list_sums (position + 1)
	    end
		
	val lst_sums = make_list_sums 1
				      
	fun get_n (sums : int list , position : int) =
	    if (hd sums) >= sum
	    then position - 1
	    else get_n (tl sums , position + 1)
   		       
    in
	get_n (lst_sums , 1)			      
    end 
		
    
(* 9. takes an int named day, returns what month that day is in 

ASSUME: day is an int between 1 and 365 *)
fun what_month (day : int) =
    let
	val days_in_month = [31 , 28 , 31 , 30 ,
			     31 , 31 , 31 , 31 ,
			     30 , 30 , 30 , 31]
    in
	(* add one to n as function returns n where first n elements in
	 list add to less than sum *)
	 number_before_reaching_sum (day , days_in_month) + 1
    end 
    
(* 10. takes two days of the year day1 and day2, returns an int list
[m1, m2, ..., mn] where:

- m1 is the month of day1
- m2 is the month of day1 + 1 
- mn is the month of day2 *)
fun month_range (d1 : int , d2 : int) =
    let
	fun make_lst (day : int) =
	    if day = d2
	    then
		[what_month (d2)]
	    else
		what_month (day) :: make_lst (day + 1)
	(* again unnecessary fun make_lst *)
    in
	if d1 > d2
	then []
	else make_lst (d1)
    end 

(* 11. takes a list of dates and evaluates to an (int*int*int) option *)
fun oldest (dates : (int*int*int) list ) =
    if null dates
    then NONE
    else
	let
	    fun oldest_nonempty (dates : (int*int*int) list) =
		if null (tl dates)
		then hd dates
		else let val tl_date = oldest_nonempty (tl dates)
		     in
			 if is_older (hd dates , tl_date)
			 then hd dates
			 else tl_date
		     end
	in
	    SOME (oldest_nonempty dates)
	end
	
	    	
(* 1. is_older evalutes true if d1 comes before d2.
   If d1 = d2, evaluates to false

fun is_older (d1 : int*int*int , d2 : int*int*int)
 *)

