(* 1 *)
fun is_older (d1 : (int*int*int), d2 : (int*int*int)) = 
  (#1 d1 < #1 d2) orelse
  (#1 d1 = #1 d2) andalso (#2 d1 < #2 d2) orelse
  (#1 d1 = #1 d2) andalso (#2 d1 = #2 d2) andalso (#3 d1 < #3 d2)

(* 2 *)
fun number_in_month (dates : (int*int*int) list, month : int) = 
  if null dates
  then 0
  else if #2 (hd dates) = month
       then 1 + number_in_month(tl dates, month)
       else 0 + number_in_month(tl dates, month)

(* 3 *)
fun number_in_months (dates : (int*int*int) list, months : int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4 *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else if #2 (hd dates) = month
       then hd dates :: dates_in_month(tl dates, month)
       else dates_in_month(tl dates, month)

(* 5 *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* 6 *)
fun get_nth (items : string list, n : int) =
  if n = 1
  then hd items
  else get_nth(tl items, n - 1)

(* 7 *)
fun date_to_string (date : (int*int*int)) = 
  let val months = ["January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November",
                    "December"]
  in 
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* 8 *)
fun number_before_reaching_sum (sum : int, numbers : int list) =
  if null numbers orelse sum - (hd numbers) <= 0
  then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

(* 9 *)
fun what_month (day : int) = 
  let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum(day, days_in_month)
  end

(* 10 *)
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest (dates : (int*int*int) list) =
  if null dates
  then NONE
  else let
    fun oldest_nonempty (dates : (int*int*int) list) =
      if null (tl dates)
      then hd dates
      else let val tl_oldest = oldest_nonempty(tl dates)
           in
             if is_older(hd dates, tl_oldest)
             then hd dates
             else tl_oldest
           end
    in
      SOME (oldest_nonempty dates)
    end

(* 12 *)
fun is_in (n : int, lst : int list) =
  if null lst
  then false
  else if n = (hd lst)
       then true
       else is_in(n, tl lst)

fun dedup (lst : int list) =
  let fun dedup_with_seen(lst : int list, seen : int list) =
    if null lst
    then []
    else if not(is_in(hd lst, seen))
         then hd lst :: dedup_with_seen(tl lst, hd lst :: seen)
         else dedup_with_seen(tl lst, seen)
  in
    dedup_with_seen(lst, [])
  end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) = 
  number_in_months(dates, dedup(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
  dates_in_months(dates, dedup(months))

(* 13 *)
fun get_nth_int (items : int list, n : int) =
  if n = 1
  then hd items
  else get_nth_int(tl items, n - 1)

fun is_leap_year(year : int) =
  (year mod 400) = 0 orelse ((year mod 4) = 0 andalso (year mod 100) <> 0)

fun reasonable_date (date : (int*int*int)) =
  let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    (#1 date) > 0
    andalso (#2 date) > 0
    andalso (#2 date) < 13 
    andalso if is_leap_year(#1 date) andalso (#2 date) = 2
            then (#3 date) <= 29
            else (#3 date) <= get_nth_int(days_in_month, #2 date)
  end
