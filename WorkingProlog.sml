val termlist: ((Term * Term) list) ref = ref [];
fun AddTerm(t1) = (termlist := !termlist @ [t1]);
fun CleanTL() = (termlist := []);
(*This is the Unification and Substitution steps*)
(*************************************************************************************)
type Substitution = string * int  -> Term;

val empty : Substitution = fn x => Var x;

fun value S (Var v)         = ((S v))                        |    
    value S (Fun (f, args)) = (Fun (f, map (value S) args)); 
 
fun comp (S, R) = fn v => value S (R v);
       
fun upd (v, t) S = comp (fn w => if w = v then t else Var w, S);
       
exception non_unifiable and occurs_check and length and next_in_list and non_solvable;
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

fun 	DoSomething2(nil, _)  = ""       |
	DoSomething2(_, nil)  = ""       |
	DoSomething2(xh::xt,zh::zt) =
	(
		AddTerm(xh,zh);
		DoSomething2(xt,zt)
	); 
				
fun 
   DoSomething(Fun(x,xlist),Fun(z,zlist)) =
	(
		DoSomething2(xlist,zlist)	      
	);

fun 
   Solve (y, x as (Headed(headedclausehead,headedclausetail))) =
	(
	   OutLine("Attempting to unify "^PrintTerm y^" with "^PrintTerm headedclausehead^"...");	(*DB*)
	   let 
		val S = unify(y,headedclausehead)
	     		handle non_unifiable => (
				      OutLine("This unification was unsuccessful. Backtracking...");
	    			      raise next_in_list) 
	    in
	   	OutLine("Unification of " ^PrintTerm y^" with "^PrintTerm headedclausehead^" succeed!");
	   	if headedclausetail=nil then 
					  (OutLine("We unified a fact! ");
					   OutLine("About to call value on "^PrintTerm y);
					   DoSomething(y, (value S y))
					  )
	 				else 
					   (OutLine("Oh no! A rule!..");
					    DoSomething(y, (value S y))
					   )
	   end	
	)
  | Solve (y, _) = 
        (
	  raise next_in_list
          (* OutLine("Something went wrong...");                (*DB*)
	   OutLine("Attempted to unify "^ PrintTerm y)*)        (*DB*)
        );













(*Once we have selected the element of our query list to check, we will then call unify with each database entry until a success. Since our database is a list of HornClauses, we must extract the head term of each horn clause. This may be redundant for facts, but it is necessary for rules, which may take the form of: 
	e.g. a(X,f(X) :- b(X)
We will need to first call unify on the head of these Headed clauses, then apply this substitution to each member of the tail. 
*)
fun 
    OpenHornClause (y, dbhead::dbtail : HornClause list) =
	(
           OutLine("Entering Solve...");                               (*         DB           *)
	   OutLine("---------------------------");
	   OutLine("Variables:  Type      Value");
	   OutLine("   y        Term      "^ PrintTerm y);
	   OutLine(" dbhead   HornClause  "^ PrintClause dbhead);
	   OutLine("---------------------------");                     (*        ENDDB          *)
	   Solve (y, dbhead)
	    handle next_in_list => if dbtail = nil then raise non_solvable else OpenHornClause (y,dbtail)
				(*This probably won't get handled here*)
	);












(*This function will open our query list. 
  We will call unify on every term in our database.
  We will then call this function again for every element in our query list *)
fun 
    OpenY (nil, _) = OutLine("Y is nil! This means we've gone through all our Ys!")  |
    OpenY (_, nil) = OutLine("What? Database is nil? uhhh...                     ")  |
    OpenY (yhead::ytail, db : HornClause list) = 
	(
	  OutLine("Entering OpenHornClause...");                    (*DB*)
	  OpenHornClause(yhead,db)
	    handle non_solvable => raise non_solvable;(*OutLine("Nope! No success for you!");*)
	  OpenY(ytail,db)
	);











(*Called from Prolog when someone types in a query*)
fun OutQuery (y : Term list, database : HornClause list) =
	(
          OutLine ("Entering OpenY..."); (*DB*)
	  OpenY(y,database)
	   handle non_solvable => raise non_solvable
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
     OutLine ("Entering OutQuery..."); (*DB*)
     OutQuery (y, !db)
      handle non_solvable => OutLine ("No");
     OutSol(!termlist)
    );
