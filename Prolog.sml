val termlist: ((Term * Term) list) ref = ref [];
fun AddTerm(t1) = (termlist := !termlist @ [t1]);
fun CleanTL() = (termlist := []);

(*This is the Unification and Substitution steps*)
type Substitution = string * int  -> Term;

val empty : Substitution = fn x => Var x;

fun value S (Var v)         = ((S v))                        |    
    value S (Fun (f, args)) = (Fun (f, map (value S) args)); 
 
fun comp (S, R) = fn v => value S (R v);
       
fun upd (v, t) S = comp (fn w => if w = v then t else Var w, S);
       
exception non_unifiable and solved and occurs_check and length and next_in_list and non_solvable;
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
                (Var v,Var w)           => if (v=w) then S else upd(v,t2')S                        |
                (Var v, _)              => if occurs v t2 then raise occurs_check else upd(v,t2')S |
                (_, Var w)              => if occurs w t1 then raise occurs_check else upd(w,t1')S |
                (Fun (f1, args1), Fun (f2, args2)) =>
                        if f1=f2 then foldr unify' S (pairup (args1, args2)) else raise non_unifiable
    end;

fun unify (t1, t2) = 
  ( unify' ((t1, t2), empty)
    handle occurs_check => raise non_unifiable
         | length       => raise non_unifiable 
 );	
(************************************************************************************************)
(*This is the meat of the solving - we call unify with the term from y and each head term in the database.*)
(* Find any instance of variable in variable, and replace with the substitution *)

	
fun Search(st : Term, []) = false | Search(st : Term, (tlh1,tlh2)::tlt : (Term * Term) list) = if (st=tlh1) then true else Search(st,tlt);
fun P3(xh::xt : Term list,zh::zt : Term list) = if xt = [] then (if not(Search(xh,!termlist)) then AddTerm(xh,zh) else ()) else (if not(Search(xh,!termlist)) then (AddTerm(xh,zh); P3(xt,zt)) else (P3(xt,zt))); 
fun P2(Fun(x,xlist),Fun(z,zlist)) = P3(xlist,zlist);
fun P1(yh, S) =
	P2(yh, (value S yh));
	  

fun 
    OpenY (y as (yhead::ytail) , ((Headed(hch,hct))::dbtail), database ,querylist) =
	(
	let val S = unify(yhead,hch)
		  handle non_unifiable => (if(dbtail = []) then raise non_solvable else OpenY(y,dbtail,database,querylist))  
	in
	     if (not(ytail = nil)) 
	     then 
		OpenY((map (value S) ytail) : Term list, database, database, querylist)
		  handle non_solvable =>if (dbtail = []) then raise non_solvable else OpenY(y,dbtail,database,querylist)
		   |     solved       => (P1(yhead,S); raise solved)
		 
		 
             else (P1(yhead, S); raise solved)
	end
       	); 

	
fun OutQuery (y : Term list, database as (dbh::dbt) : HornClause list) =
	(
	  OpenY(y,database,database,y)
	   handle solved => raise solved
		 | non_solvable => if dbt = [] then raise non_solvable else OpenY(y,dbt,database,y);
	   OutLine("EndingOutQuery")
 	);


fun Prolog (x as (Headed (Var _, _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (x as (Headless (Var _ :: _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (Headed (Fun ("init", nil),nil)) = InitDB ()
  | Prolog (x as (Headed _)) = Assert x
  | Prolog (x as Headless y) =
    (
     CleanTL();  
     OutLine ("query:  " ^ PrintClause x);
     (*OutLine ("Entering OutQuery..."); (*DB*)*)
      OutQuery (y, !db)
      handle non_solvable => OutLine ("No")
      |        solved     => OutSol (!termlist)
   );



