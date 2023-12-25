(* Homework Assignment 1 *)

(* Question 1 *)

fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2 then true
    else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2 then true
    else if #1 date1 < #1 date2 then true
    else false

(* Question 2 *)
								      
fun number_in_month (xs : (int * int * int) list, month : int) =
    if null xs
    then 0
    else 
	if # 2 (hd xs) <> month
	then 0 + number_in_month (tl xs, month)
	else 1 + number_in_month (tl xs, month)
	    
(* Question 3 *)

fun number_in_months (xs : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (xs, hd months) + number_in_months (xs, tl months)

(* Question 4 *)

fun dates_in_month (xs : (int * int * int) list, month : int) =
    if null xs
    then []
    else
	if #2 (hd xs) = month
	then (hd xs)::dates_in_month (tl xs, month)
	else dates_in_month (tl xs, month)

(* Question 5 *)
	    
fun dates_in_months (xs : (int * int * int) list, months : int list) =
    if null xs orelse null months
    then []
    else
	dates_in_month (xs, hd months) @ dates_in_months (xs, tl months)

(* Question 6 *)
							 
fun get_nth (xs : string list, n : int) =
    if n=1
    then hd xs
    else get_nth (tl xs, (n - 1))

(* Question 7 *)

fun date_to_string (date : (int * int * int)) =
     
    get_nth (["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)

(* Question 8 *)

fun number_before_reaching_sum (sum : int, xs : int list) =
    if hd xs >= sum
    then 0
    else 1 + number_before_reaching_sum ((sum - (hd xs), tl xs))

(* Question 9 *)

fun what_month (day_of_year : int) =
    1 +  number_before_reaching_sum (day_of_year, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

(* Question 10 *)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else if day1 = day2 then what_month(day1)::[]
    else what_month(day1)::month_range (day1 + 1, day2)

(* Question 11 *)

fun oldest (xs : (int * int * int) list) =
    if null xs
    then NONE
    else
	let
	    fun oldest_nonempty (xs: (int * int * int) list) =
		if null (tl xs)
			 then hd xs
			 else let val d = oldest_nonempty(tl xs)
			      in
				  if is_older(d, hd xs)
				  then d
				  else hd xs
			      end
	in
	    SOME (oldest_nonempty xs)
	end

(* Challenge questions *)	

(* Question 12 *)

(* First create function to remove duplicates from list.
Created separately as used in both number_in_months_challenge 
and dates_in_months_challenge. *)

fun remove_duplicates (months: int list) =

    let
	fun check_if_on_list (months : int list, month : int) =
	    if null months
	    then false
	    else (month = hd(months) orelse check_if_on_list (tl(months),month))

    in
	
    if null months
    then []
    else if
	check_if_on_list (tl(months), hd(months))
	   then
	       remove_duplicates (tl(months))
    else hd(months) :: remove_duplicates (tl(months))
	
    end
 

fun number_in_months_challenge (xs : (int * int * int) list, months : int list) =

    let
	val months_no_duplicates = remove_duplicates (months)
    in
	number_in_months (xs, months_no_duplicates)
    end

fun dates_in_months_challenge (xs : (int * int * int) list, months : int list) =

    let
	val months_no_duplicates = remove_duplicates(months)
    in
	dates_in_months (xs, months_no_duplicates)
    end    
	    
(* Question 13 *)

fun reasonable_date (date : int*int*int) =

((#2 date <> 2 orelse #3 date <> 29) orelse ((#1 date mod 400 = 0) orelse ((#1 date mod 4 = 0) andalso (#1 date mod 100 <> 0))))
						andalso
						#1 date >= 1
						andalso
						(#2 date >=1 andalso # 2 date <=12)
						andalso
let 
fun get_days_in_month (xs : int list, n : int) =
    if n=1
    then hd xs
    else get_days_in_month (tl xs, (n - 1))

in
						(#3 date >= 1 andalso #3 date <= get_days_in_month([31,29,31,30,31,30,31,31,30,31,30,31], #2 date))
end
					
