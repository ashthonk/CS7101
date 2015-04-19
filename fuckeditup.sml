(*TODO*)
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
fun addone (Var(a:string,b:int)) = (Var(a,b+1));
fun 
    value S (Var v) = (S v) |
    value S (Fun(f,args)) = (Fun (f,map (value S) args)); 
fun comp (S, R) = ( fn v => value S (R v));
       
fun upd (v, t) S = ( comp (fn w => if w = v then t else Var w, S));
       
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
 
fun unify' ((t1,t2),S) =
    let
        val t1' = top S t1 and t2' = top S t2;
    in
        case (t1', t2') of
                (Var v,Var w)        => if (v=w) then (upd(v,addone(t2))S) else (upd(v,addone(t2'))S )                      |
                (Var v, _)           => if occurs v t2 then raise occurs_check else upd(v,t2')S |
                (_, Var w)           => if occurs w t1 then raise occurs_check else upd(w,t1')S |
                (Fun (f1, args1), Fun (f2, args2)) =>
                        if f1=f2 then foldr unify' S (pairup (args1, args2)) else raise non_unifiable
    end;

fun unify (t1, t2) = 
   unify' ((t1, t2), empty)
        handle occurs_check => raise non_unifiable
         | length       => raise non_unifiable;





(*fun Consolidate(((Var v),(Var w))::tl) = (
    C2(th,tl)
)*)
(*Search and P1-P3 will navigate the list of terms and clean out any duplicate terms prior to the final output. This can probably be done much more optimally, but I will re-iterate my novice status of functional programming languages here)*)
fun Search(st : Term, []) = false | Search(st : Term, (tlh1,tlh2)::tlt : (Term * Term) list) = if (st=tlh1) then true else Search(st,tlt);
fun P3(xh::xt : Term list,zh::zt : Term list) = 
		if xt = [] 
		then (if not(Search(xh,!termlist)) 
		      then if not(xh=zh) 
                           then AddTerm((xh,zh),termlist) 
                           else () 
                      else ()) 
               else (if not(Search(xh,!termlist)) 
                     then (if not(xh=zh) 
                           then AddTerm((xh,zh),termlist)
                           else (); 
                           P3(xt,zt)) 
                     else (P3(xt,zt))); 
fun P2(Fun(x,xlist),Fun(z,zlist)) = P3(xlist,zlist);
fun P1(yh, S) = P2(yh, (value S yh));

fun Search'(st : Term, []) =() | Search'(st : Term,(tlh1,tlh2)::tlt : (Term * Term) list) = (OutLine("DBBBB:::::   "^PrintTerm(st)^" "^PrintTerm(tlh1)); if (st=tlh1) then AddTerm((tlh1,tlh2),unabridgedtl) else Search'(st,tlt));
fun RDBS3 (ah::at,tl) = (Search'(ah,tl); if (not(at = [])) then RDBS3(at,tl) else ());
fun RDBS2(Fun(f,args),termlist) = RDBS3(args,termlist);
fun RuleDBSanatize(yhead,termlist) = (RDBS2(yhead,termlist));
	

(*This is the solver*)	  
fun 
    Solver (y as (yhead::ytail), ((Headed(hch,hct))::dbtail), database,querylist)=
	(

	if(hct = nil)
         then( 	
	 
	 let val S = unify(yhead, hch)
		  handle non_unifiable => (if(dbtail = []) then (raise non_solvable) else (Solver(y,dbtail,database,que          rylist ))) 
	 in
	      (*Fact*)
	     (if (not(ytail = nil)) 
	     then (
		
		Solver((map (value S) ytail) : Term list, database, database, querylist)
		  handle non_solvable =>if (dbtail = []) 
					then (
					      raise non_solvable) 
                                        else ( 
                                              Solver(y,dbtail,database,querylist))
		   |     solved       => (P1(yhead,S);  raise solved)
		 )
	      else (P1(yhead, S);  raise solved)	
           end)
	   else (*Rule*) 
	   (    
		let val S = unify(hch,yhead)
			 handle non_unifiable => (if(dbtail = []) then ( raise non_solvable) else ( Solver(y,dbtail,database,querylist)));
		in
		(*Using hct as the tail of the rule,*)
               
	
	
		Solver((map (value S) hct) : Term list, database, database, querylist)
		  handle non_solvable => if (dbtail = []) then (raise non_solvable) else (Solver(y,dbtail,database,querylist))
		  |      solved       => (P1(yhead,S); (RuleDBSanatize((value S hch), !termlist);raise qsolved) )
		end          
  	   )
	
       	); 


(*End Solver*)	

(*OutQuery - Handles the initial call to the solver. Handles a few special cases as well*)
fun OutQuery (y as (yhead::ytail) : Term list, database as (dbh::dbt) : HornClause list) =
	(
	  Solver(y,database,database,y)
	   handle solved => raise solved
		 | non_solvable => if dbt = [] then raise non_solvable else Solver(y,dbt,database,y)
		 | qsolved =>  raise qsolved
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
      |        qsolved    => OutSol (!unabridgedtl) (*Can this be deleted*)
   );
