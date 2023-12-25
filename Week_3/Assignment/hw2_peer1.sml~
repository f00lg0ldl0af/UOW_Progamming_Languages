(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* test strings *)
val str1 = "Bradley";
val str_list1 = ["Alex", "Brad", "Bradley", "Mark", "Emily", "Emma"];
val str2 = "Fred";
val str3 = "Jeff";
val str_list2 = ["Fred", "Fredrick"];
val str_list3 = ["Elizabeth", "Betty"];
val str_list4 = ["Freddie", "Fred", "F"];
val str_list5 = ["Jeff", "Jeffrey"];
val str_list6 = ["Geoff", "Jeff", "Jeffrey"];

(* put your solutions for problem 1 here *)
(* 1a - all_except_option *)
fun all_except_option (str : string ,lst : string list) =
    let
	fun helper [] = NONE
	  | helper (x::xs) =
	    if same_string(x, str) then
		SOME(xs)
	    else
		case helper xs of
		    NONE => NONE
		  | SOME(rest) => SOME(x::rest)
    in
	helper lst
    end;

(* 1b - get_substitutions1 *)
fun get_substitutions1 (substitutions : string list list, s : string) =
    let
	fun helper ([], acc) = acc
	  | helper (x::xs, acc) =
	    case all_except_option (s, x) of
		NONE => helper(xs, acc)
	      | SOME(lst) => helper(xs, acc @ lst)
    in
	helper(substitutions, [])
    end;

(* 1c - get_substitutions2 *)
fun get_substitutions2 (substitutions : string list list, s : string) =
    let
	fun prepend_sublist([], acc) = acc
	  | prepend_sublist (x::xs, acc)  = prepend_sublist(xs, x::acc)
	fun helper([],acc) = List.rev acc
	  | helper (x::xs, acc) =
	    case all_except_option(s, x) of
		NONE => helper(xs, acc)
	      | SOME(lst) => helper(xs, prepend_sublist(lst, acc))
    in
	helper(substitutions, [])
    end;

(* 1d - similar_names *)
(* first define fullname datatype *)
type fullname = {first: string, middle: string, last: string};

fun similar_names (substitutions: string list list, {first, middle, last}: fullname) =
    let
	fun make_fullnames(subst_first_names, acc) =
	    case subst_first_names of
		[] => acc
	      | x::xs => make_fullnames(xs, acc @ [{first=x, middle=middle, last=last}])
	val subst_first_names = get_substitutions2(substitutions, first)
    in
	make_fullnames(subst_first_names, [{first=first, middle=middle, last=last}])
    end;





(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int ;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw ;

exception IllegalMove;

(* testing variables *)
(* sample cards *)
val card1 = (Hearts, Ace);
val card2 = (Hearts, Num 7);
val card3 = (Clubs, Queen);
val card4 = (Spades, King);
val card5 = (Diamonds, Num 10);

(* lists of cards *)
val cards = [card1, card2, card3, card4, card5]; (* all cards *)
val same_color_cards = [card1, card2, card5]; (* all red *)


(* put your solutions for problem 2 here *)
(* 2a - card_color *)
fun card_color (suit, _)= case suit of
			      Clubs => Black
			    | Spades => Black
			    | Diamonds => Red
			    | Hearts => Red;

(* 2b - card_value *)
fun card_value (_, rank)= case rank of
			      Num n => n
			    | Ace => 11
			    | _ => 10;

(* 2c - remove_card *)
fun remove_card (cs, c) =
    let
	fun helper([], _) = raise IllegalMove
	  | helper (x::xs, true) = x::helper(xs, true)
	  | helper (x::xs, false) =
	    if x = c then xs
	    else x::helper(xs, false)
    in
	helper(cs, false)
    end;

(* 2d - all_same_color *)
fun all_same_color cards =
    case cards of
	[] => true
      | (suit, _)::rest  =>
	let
	    fun helper color [] = true
	      | helper color ((suit, _)::xs) =
		(card_color (suit, Num 0) = color) andalso helper color xs
	in
	    helper (card_color (suit, Num 0)) rest
	end;

(* 2e - sum_cards *)
fun sum_cards cards =
    let
	fun sum_helper([], acc) = acc
	  | sum_helper ((suit, rank)::xs, acc) = sum_helper(xs, acc + card_value(suit,rank))
    in
	sum_helper(cards, 0)
    end;

(* 2f - score *)
fun score (heldCards : card list, goal : int) =
    let
	val sum = sum_cards heldCards
	val sameColorBonus = all_same_color heldCards
	val prelimScore = if sum > goal then (sum-goal) * 3 else goal - sum
	val finalScore = if sameColorBonus then prelimScore div 2 else prelimScore
    in
	finalScore
    end;

(* 2g - officiate *)
fun officiate (cardList : card list, moves : move list, goal : int) =
    let
	fun game_helper (heldCards, [], _) = score(heldCards, goal)
	  | game_helper (heldCards, move::restMoves, cards) =
	    case move of
		Discard c =>
		if List.exists (fn x => x = c) heldCards then
		    game_helper (remove_card(heldCards, c ), restMoves, cards)
		else
		    raise IllegalMove
	      | Draw =>
		case cards of
		    [] => score(heldCards, goal)
		  | card::restCards =>
		    let
			val newHeldCards = card::heldCards
			val newSum = sum_cards newHeldCards
		    in
			if newSum > goal then score(newHeldCards, goal)
			else game_helper(newHeldCards, restMoves, restCards)
		    end
    in
	game_helper([], moves, cardList)
    end;
