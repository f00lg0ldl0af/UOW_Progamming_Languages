(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a *)
fun all_except_option (string, string_list) =
    case string_list of
	[] => NONE
      | x::xs'  => case same_string(x, string) of
		       true => SOME xs'
		     | false => case all_except_option(string, xs') of
				    NONE => NONE
				  | SOME tail  => SOME (x::tail)

(* 1b *)
fun get_substitutions1 (substitutions, s)=
    case substitutions of
	[] => []
      | list::rest=> let val lstOption = all_except_option(s, list)
		  in case lstOption of
			 NONE => get_substitutions1(rest, s)
		       | SOME lst => lst @ get_substitutions1(rest, s)
		  end

(* 1c *)
fun get_substitutions2 (substitutions, s) =
    let fun helper (substitutions, acc) =
	    case substitutions of
		[] => acc
		| list::rest=> let val lstOption = all_except_option(s, list)
		  in case lstOption of
			 NONE => helper(rest, acc)
		       | SOME lst => lst @ helper(rest, acc)
			       end
    in
	helper (substitutions, [])
    end

(* 1d *)
fun similar_names (substitutions,full_name) =
    let 
        val {first=f, middle=m, last=l} = full_name
	      fun make_names xs =
	         case xs of
		   [] => []
	           | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
    in
	      full_name::make_names(get_substitutions2(substitutions,f))
    end


       		    
    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* put your solutions for problem 2 here *)

(* 2a *)
fun card_color card =
    case card of
	Spades => Black
      | Clubs => Black
      | Hearts  => Red
      | Diamonds => Red
			
(* 2b *)	      
fun card_value card =
    case card of
	Num(int) => int
      | Ace => 11
      | _ => 10
		 
 (* 2c *)
fun remove_card (cs, c, e) =
    let
	fun remove_onlyone([], _, _) = raise e
	  | remove_onlyone (x::xs, c, _) =
	    if x = c then xs
	    else x::remove_onlyone(xs, c, e)
    in
	remove_onlyone(cs, c, e)
    end;

(* 2d *)
fun all_same_color cards =
    case cards of
        [] => true
      | [_] => true
      | (suit1, _) :: rest =>
            case rest of
                (suit2, _) :: _ =>
                    if card_color suit1 = card_color suit2 then
                        all_same_color rest
                    else
                        false
             | _ => false;

(* 2e *)
fun sum_cards cards =
    let
        fun sum_helper([], acc) = acc
          | sum_helper((_, card)::rest, acc) = sum_helper(rest, acc + card_value card)

    in
        sum_helper(cards, 0)
    end;

(* 2f *)
fun score (c_list, goal) =
    let
	fun preliminary_score() =
	    if sum_cards c_list > goal
	    then 3 * (sum_cards c_list - goal)
	    else goal - sum_cards c_list
    in
	if all_same_color c_list
	then preliminary_score() div 2
	else preliminary_score()
    end;

				 
fun officiate (clist, mlist, goal) =
	      let fun turn (hlist, clist, mlist) =
		      case mlist of
			  [] => score(hlist, goal)
			| Draw :: rest =>(case clist of
					      [] => score(hlist, goal)
					    | card :: newclist => let val newhlist = card :: hlist
								  in turn (newhlist, newclist, rest)
								  end)
		        | Discard c :: rest => let val newhlist = remove_card(hlist, c, IllegalMove)
                                      in turn (newhlist, clist, rest)
                                      end
	      in
		  turn ([], clist, mlist)
	      end;
