(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

(* Q1 ASSUME: all strings have at least 1 character *)
val only_capitals = List.filter (fn x => (Char.isUpper (String.sub (x,0)))) 

(* Q2 returns longest string in list, else return "" if list is empty.
Else if tie, return string closest to beginning of list *)
val longest_string1 = foldl (fn (s1,s2) => if String.size s1 > String.size s2 then s1 else s2) "" 
	  
(* Q3 if tie, return string closest to end of list. *)
val longest_string2 = foldl (fn (s1,s2) => if String.size s1 >=  String.size s2 then s1 else s2) "" 
(* Q4  fn: (int * int -> bool) -> string list -> string
 - longest_string_helper is passed function that behaves like >
 - (so it returns true when 1st arg > 2nd arg) *)
	  
fun longest_string_helper f = foldl (fn (s1,s2) => if f(String.size s1,String.size s2) then s1 else s2) "" 

val longest_string3 = longest_string_helper (fn (s1,s2) => s1 > s2)

val longest_string4 = longest_string_helper (fn (s1,s2) => s1 >= s2)
	  
(* Q5  *)
val longest_capitalized = longest_string3 o only_capitals	  
	  
(* Q6 *)
val rev_string = String.implode o List.rev o String.explode	  

(* Q7 fn: ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f xs = case xs of
			    [] => raise NoAnswer
			  | x::xs' => case f x of SOME v => v
						| NONE => first_answer f xs'			  			    				 						 
(* Q8 fn: ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f lst =
    let fun aux (lst,acc) = case lst of
				 [] => SOME acc
			     | x::xs => case f x of
					     SOME lstn => aux(xs,lstn@acc)
					   | NONE => NONE
    in aux(lst,[])
    end

(* Provided Code *)	
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

						
datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
	
(* Q9a *)
val count_wildcards = g (fn () => 1) (fn _ => 0) 
			  
(* Q9b *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size 
			  
(* Q9c count_some_var ("x", Variable("x")) = 1
returns no. of times string appears as a variable in pattern *)
fun count_some_var (s,p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p
					
(* Q10 Returns true if all variables (strings) in pattern are distinct.
- 1st helper: takes a pattern and returns list of strings used in variables.
- 2nd helper: takes list of strings and decide if it has repeats. *)
fun check_pat p =
    let	fun helper1 p  = case p of
			     Variable s => [s]
			   | TupleP ps => foldl (fn (p,i) => helper1 p@i) [] ps
			   | ConstructorP (_,p) => helper1 p
			   | _ => []	     
			     
	fun helper2 los = case los of 
			      [] => true (* no duplicates *)
			    | s::ss => (not (List.exists (fn x => x = s) ss)) andalso helper2 ss
    in
	(helper2 o helper1) p
    end

(* Q11 valu * pattern -> (string * valu) list option
- if match, return SOME lst (list of bindings)
- if match but pattern has no patterns of form Variable s, return SOME []
- else, return NONE

 all_answers type fn: ('a -> 'b list option) -> 'a list -> 'b list option
- 1st arg applied to 2nd arg
- if returns NONE for any element, then result for all_answers is NONE
else produce SOME lst1 ... SOME lstn and result is SOME lst when lst is lst1,lst2,...lstn appended together

Note: all_answers and ListPair.zip are useful
zip(l1,l2) combines two lists into list of pairs, first elements of both lists is the first element of the result *)
fun match (v,p) = case (v,p) of
		      (_,Wildcard) => SOME []
		    | (_,Variable s) => SOME [(s,v)]
		    | (Unit,UnitP) => SOME []
		    | (Const x,ConstP y) => if x = y then SOME [] else NONE
		    | (Tuple vs,TupleP ps) => if List.length vs = List.length ps
					      then all_answers match (ListPair.zip(vs,ps))
					      else NONE
		    | (Constructor(s2,v'),ConstructorP(s1,p')) => if s1 = s2
								  then match(v',p')

								  else NONE
		    | _ => NONE

			       
(* Q12 fn: valu -> pattern list -> (string * valu) list option
- returns NONE if no pattern in list matches
- returns SOME lst for first pattern in list that matches 

fun first_answer f xs =
    case xs of [] => raise NoAnswer
    	      | x::xs' => case f x of SOME v => v
	      	       	       	      | NONE => first_answer f xs' *)	
fun first_match v ps = SOME (first_answer (fn p => match (v,p)) ps) handle NoAnswer => NONE 

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string    

(* Challenge "type-checks" a pattern list i.e., create an algorithm that infers the type t of x based on patterns p1, p2, ..., pn

fn: ((string * string * typ) list) * (pattern list) -> typ option.

- 1st argument ("foo","bar",IntT) i.e., constructor foo makes a value of type Datatype "bar" given value of type IntT
contains a type list
	 [ ("Red", "color", UnitT),
	   ("Green", "color", UnitT),
	   ("Blue", "color", UnitT)
	 ]
 
- 2nd argument contains the patterns p1, p2, ..., pn
- If all the patterns in the case expression are compatible with some type t
then the answer is SOME t, otherwise NONE.
 *)									    
fun typecheck_patterns (ds, ps) =
    let fun tcheck p acc =
	    let fun cycleT ps ts acc = case ps of
					   [] => acc
					|  p::ps' => cycleT ps' (tl ts) (acc @ [valOf (tcheck p (SOME (hd ts)))] )

		fun cycleTA ps acc = case ps of
					 [] => acc
				       | p::ps' => cycleTA ps' (acc @ [valOf (tcheck p (SOME Anything))] )

		fun validd dname ds pat = case ds of
					      [] => raise NoAnswer
					    | (dcons,dtype,dpat)::ds' => if (dname = dcons) andalso isSome (tcheck pat (SOME (dpat)))
									 then (Datatype dtype)
									 else validd dname ds' pat
	    in (* run through iterations with different datatype patterns p*)
		case (p,acc) of (Wildcard,SOME Anything) => SOME Anything
				| (Wildcard, SOME UnitT) =>  SOME UnitT
				| (Wildcard, SOME IntT) => SOME IntT
				| (Wildcard, SOME (TupleT ps)) => SOME (TupleT ps)
				| (Wildcard, SOME (Datatype s)) => SOME (Datatype s)
									
				| (Variable _,SOME Anything) => SOME Anything | (Variable _, SOME UnitT) => SOME UnitT (* don't get this *)		
				| (Variable _, SOME IntT) => SOME IntT
				| (Variable _, SOME (TupleT ps)) => SOME (TupleT ps)
				| (Variable _, SOME (Datatype s)) => SOME (Datatype s)

				| (UnitP,SOME UnitT) => SOME UnitT
				| (UnitP, SOME Anything) => SOME UnitT
				| (ConstP i, SOME IntT) => SOME IntT
				| (ConstP i, SOME Anything) => SOME IntT
									      
				| (TupleP (ps), SOME Anything) => SOME (TupleT (cycleTA ps [])) (* cycleTA to run through the patterns in ps *) 
				| (TupleP (ps), SOME (TupleT (ts))) =>
						  if List.length ps = List.length ts then SOME (TupleT (cycleT ps ts [])) (* cycleT *)
						  else raise NoAnswer
							     
				| (ConstructorP(s, pat), SOME Anything) => SOME (validd s ds pat) (*validd*)

				| (ConstructorP(s, pat), SOME (Datatype d)) => if (validd s ds pat) = (Datatype d)
									       then SOME (Datatype d)
									       else raise NoAnswer

				| (_,_) => raise NoAnswer
	    end

	fun cycle ps acc = case ps of [] => acc
				    | p::ps' => cycle ps' (tcheck p acc) (* check the next pattern against the type of the previous pattern *)
    in
	if null ps then NONE
	else cycle ps (SOME (Anything)) handle NoAnswer => NONE
    end
			       
