(*  Abstract syntax of Prolog.  *)

structure Syntax : SYNTAX =
    struct

	datatype Term =
	  Fun of string * Term list |
	  Var of string * int       ;

	datatype HornClause =
	  Headed of Term * Term list    |
	  Headless of Term list         ;

	fun
	  PrintList nil		 = ""	|
	  PrintList (head::tail) = 
	    foldr op^ ""
	      ((PrintTerm head)::(map (fn x => "," ^ (PrintTerm x)) tail)) 
	and
	  PrintTerm (Fun (s,nil)) = s  (* s is a constant *)		        |
	  PrintTerm (Fun (s,list))= foldr op^ "" [s, "(", (PrintList list),")"]	|
          (* "parse" assigns all variables to have instance number equal to 0. *)
	  PrintTerm (Var (s,0))   = s					        |
	  PrintTerm (Var (s,n))   =
	    foldr op^ "" ["_", s, (Int.toString n)]	;

	fun PrintDB nil          = "Empty Database :("   |
	    PrintDB (head::tail) = 
	     foldr op^ ""
		((PrintClause head)::(map (fn x=> "," ^ (PrintClause x)) tail))
	and
	  PrintClause (Headed (d,nil)) = foldr op^ "" [PrintTerm d, "." ]	|
	  PrintClause (Headed (d,l))   =
            foldr op^ "" [PrintTerm d, " :- ", PrintList l, "." ]		|
	  PrintClause (Headless nil)   = "False ?"			|
	  PrintClause (Headless l)     = foldr op^ "" [ PrintList l, "?" ]	;

        fun OutLine(x) = print ( x ^ "\n" );

	local
	  fun OutPair(x,y) =
	    OutLine (foldr op^ "" [ (PrintTerm x), " = ", (PrintTerm y) ]);
	in
	  fun OutSol list =
	      (
	        OutLine ("solution:");
                app OutPair list
	      );
	end;
	(*Debugging*)
	
	(*End Debugging*)        



	val db: (HornClause list) ref = ref [];
	fun InitDB()   = (db := []; OutLine "Database erased.")
	fun Assert(cl) = (db := !db @ [cl]; OutLine("assert:  "^ PrintClause cl));
    end;
