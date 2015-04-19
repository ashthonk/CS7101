(*CRITICAL
  	Fix rule handling
	Fix data output
  IDEAL
	Implement better recursive handling of S
  LP
	Cut/Fail
*)

val termlist: ((Term * Term) list) ref = ref [];
val unabridgedtl: ((Term * Term) list) ref = ref[];
fun AddTerm(t1,termlist) = (termlist := !termlist @ [t1]);
fun CleanTL(tl) = (tl := []);

(*This is the Unification and Substitution steps*)
type Substitution = string * int  -> Term;

val empty : Substitution = fn x => Var x;
fun addone (Var(a:string,b:int)) = (OutLine("Addone called");Var(a,b+1));
fun 
    value S (Var v) = (S v) |
    value S (Fun(f,args)) = (Fun (f,map (value S) args)); 
fun comp (S, R) = ((OutLine ("Comp called"); fn v => value S (R v)));
       
fun upd (v, t) S = (OutLine("Upd called for "^PrintTerm(Var v) ^ " and "^PrintTerm(t)); comp (fn w => if w = v then t else Var w, S));
fun upd''((Var v)::at,bh::bt) = 
       ( 
	OutLine("Attempting to re-pair "^PrintTerm(Var v)^ " and "^PrintTerm bh);	
	let val V = empty
	    val W = comp(V, (upd(v,bh)V)) 
	in
	if not(at = []) then comp(W,upd''(at,bt)) else W
        end )
 | upd''((Fun(v,args))::at,bh::bt) =
       ( 
	empty
        );
 
fun upd'(Fun(f, args1),Fun(y,args2)) = (OutLine("upd' called for "^PrintTerm(Fun(f,args1))^":"^PrintTerm(Fun(f,args2))); upd''(args1,args2));
	
	
fun LevelUp(Var(x,y),level) = (OutLine("Leveling up "^x^" to "^PrintTerm(Var(x,level))); Var(x,level)) 
	|  LevelUp (Fun(f,args),level) = Fun(f, map(fn x => LevelUp(x,level)) args);
       
exception non_unifiable and solved and qsolved and occurs_check and length and next_in_list and non_solvable;
fun
  top S (Var v) = S v   |
  top S (x)     = x     ;
fun
  pairup (nil, nil)             = nil                    |
  pairup (a::b, c::d)           = (a,c)::(pairup (b,d))  |
  pairup (_)                    = raise length;
 
fun
  occurs v (Var w)         =  (v = w)                    |
  occurs v (Fun (f, args)) =  List.exists (occurs v) args;
 
fun unify' ((t1,t2),S)  =
    let
        val t1' = top S t1 and t2' = top S t2;
    in
        case (t1', t2') of
                (Var v,Var w)        => if (v=w) then (upd(v,(t2'))S) else (upd(v,(t2'))S)                      |
                (Var v, _)           => if occurs v t2 then raise occurs_check else upd(v,t2')S |
                (_, Var w)           => if occurs w t1 then raise occurs_check else upd(w,t1')S |
                (Fun (f1, args1), Fun (f2, args2)) =>
                        if f1=f2 then foldr unify' S (pairup(args1, args2)) else raise non_unifiable
    end;

fun unify (t1,t2,level,S) = 
			(
			let val R = (unify' ((t1, t2), empty))
                            handle occurs_check => raise non_unifiable
                             | length           => raise non_unifiable
			in
			OutLine("Unify was successful for "^PrintTerm t1^" and "^PrintTerm t2); 
			comp(S,(upd'(t1,LevelUp((t2),level)))) 
			end);
fun unify (t1,t2,level,nil) =
			(
			let val R = (unify' ((t1,t2),empty)
			    handle occurs_check => raise non_unifiable
				|length         => raise non_unifiable)
			in
			OutLine("Unify was successful for "^PrintTerm t1^" and "^PrintTerm t2^"::NIL"); 
			 comp(R,upd'(t1,LevelUp((t2),level)))
			end);
 
			 
       

(*Search and P1-P3 will navigate the list of terms and clean out any duplicate terms prior to the final output. This can probably be done much more optimally, but I will re-iterate my novice status of functional programming languages here)*)
fun Search(st : Term, []) = false | Search(st : Term, (tlh1,tlh2)::tlt : (Term * Term) list) = if (st=tlh1) then true else Search(st,tlt);
fun P3(xh::xt : Term list,zh::zt : Term list) = 
		if xt = [] 
		then (if not(Search(xh,!termlist)) 
		      then if not(xh=zh) 
                           then (OutLine("Adding term to termlist"); AddTerm((xh,zh),termlist)) 
                           else () 
                      else ()) 
               else (if not(Search(xh,!termlist)) 
                     then (if not(xh=zh) 
                           then (OutLine("Additng term to termlist"); AddTerm((xh,zh),termlist))
                           else (); 
                           P3(xt,zt)) 
                     else (P3(xt,zt))); 
fun P2(Fun(x,xlist),Fun(z,zlist)) = P3(xlist,zlist);
fun P1(yh, S) = (OutLine("Searching for "^ PrintTerm yh^ " paird with "^PrintTerm(value S yh)); P2(yh, (value S yh)));

fun Search'(st : Term, []) =() | Search'(st : Term,(tlh1,tlh2)::tlt : (Term * Term) list) = if (st=tlh1) then AddTerm((tlh1,tlh2),unabridgedtl) else Search'(st,tlt);
fun RDBS3 (ah::at,tl) = (Search'(ah,tl); if (not(at=[])) then RDBS3(at,tl) else ());
fun RDBS2(Fun(f,args),termlist) = RDBS3(args,termlist);
fun RuleDBSanatize(yhead,termlist) = RDBS2(yhead,termlist);
	
(*This is the solver*)	  
fun 
    Solver (y as (yhead::ytail), ((Headed(hch,hct))::dbtail), database,querylist,counter,level)=
	(

	if(hct = nil)
         then( 	
	 OutLine("Attempting to unify "^PrintTerm(yhead)^ " with "^PrintTerm(hch));
	 let val S = unify(yhead, hch,level,nil)
		  handle non_unifiable => (if(dbtail = []) then (OutLine("Didn't work, backtracking.."); raise non_solvable) else (OutLine("Stepping down in DB and trying again"); Solver(y,dbtail,database,querylist, (counter + 1),level))); 
	 in
	      (*Fact*)
	     (if (not(ytail = nil)) 
	     then (
		OutLine("Entering next Y in list -- Not necessarily stepping down "^ Int.toString(counter + 1));
		Solver((map (value S) ytail) : Term list, database, database, querylist,(counter+1),(level))
		  handle non_solvable =>if (dbtail = []) 
					then (OutLine("Backtracking to previous entry in query list");
					      raise non_solvable) 
                                        else (OutLine("Stepping down in DB and trying again"); 
                                              Solver(y,dbtail,database,querylist,(counter+1),level))
		   |     solved       => (P1(yhead,S); OutLine("Moving back one in list - Success return"); raise solved)
		 )
	      else (P1(yhead, S); OutLine("Solved all entries in Y - Success return"); raise solved))
	  end)
	   else (*Rule*) 
	   (    
		let val S = unify(hch,yhead,(level),nil)
			 handle non_unifiable => (if(dbtail = []) then (OutLine("Backtracking to previous entry in query list "); raise non_solvable) else (OutLine("Stepping down in DB and trying again"); Solver(y,dbtail,database,querylist, (counter + 1),level)));
		in
		(*Using hct as the tail of the rule,*)
                OutLine("Attempting to map value S to tail of rule...");
		OutLine(PrintTerm(yhead)^ " => "^PrintTerm(yhead)^" :- "^PrintList(map (value S) hct));
		OutLine("Working on the tail of the rule"^PrintList((map(value S) hct)));
		Solver((map (value S) hct) : Term list, database, database, querylist,(counter+1),(level+1))
		  handle non_solvable => if (dbtail = []) then (OutLine("Stepping out of the rule to previous entry in query list"); raise non_solvable) else (OutLine("Stepping down in DB and trying again"); Solver(y,dbtail,database,querylist,(counter+1),level))
		  |      solved       => (P1(yhead,S); (OutLine("Leaving: " ^ Int.toString(counter));raise qsolved))
		end          
  	   )
	
       	); 


(*End Solver*)	

(*OutQuery - Handles the initial call to the solver. Handles a few special cases as well*)
fun OutQuery (y as (yhead::ytail) : Term list, database as (dbh::dbt) : HornClause list) =
	(
	 let val count = 0
	 in 
	  OutLine("Entering: 1"); 
	  Solver(y,database,database,y,count,1)
	   handle solved => raise solved
		 | non_solvable => if dbt = [] then raise non_solvable else Solver(y,dbt,database,y,count,0)
		 | qsolved => (RuleDBSanatize(yhead,!termlist); raise qsolved);
	   OutLine("EndingOutQuery")
         end
 	);
(*End OutQuery*)

fun Prolog (x as (Headed (Var _, _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (x as (Headless (Var _ :: _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (Headed (Fun ("init", nil),nil)) = InitDB ()
  | Prolog (x as (Headed _)) = Assert x
  | Prolog (x as Headless y) =
    (
     CleanTL(termlist); 
     CleanTL(unabridgedtl);
     OutLine ("query:  " ^ PrintClause x);
      OutQuery (y, !db)
      handle non_solvable => OutLine ("No")
      |        solved     => OutSol (!termlist)
      |        qsolved    => OutSol (!termlist) (*Can this be deleted*)
   );


