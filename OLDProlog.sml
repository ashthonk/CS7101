fun Prolog (x as (Headed (Var _, _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (x as (Headless (Var _ :: _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (Headed (Fun ("init", nil),nil)) = InitDB ()
  | Prolog (x as (Headed _)) = Assert x
  | Prolog (x as Headless y) =
    (OutLine ("query:  " ^ PrintClause x);
     OutQuery (y, !db)
    )

(*The following lines are for debugging purposes only*)
fun PrintDatabase ()= 
	OutLine ("The content of the Database is:\n    "^PrintDB (!db))
(*END DEBUGGING*)

fun OutQuery (y : Term list, database : HornClause list) =
	OutLine("Hi! I'm OutQuery")

(*Begin Unification and Substitution implementation*)
type Substitution = string * int  -> Term;

val empty : Substitution = fn x => Var x;

fun value S (Var v)         = S v                        |     
    value S (Fun (f, args)) = Fun (f, map (value S) args);  
	
fun comp (S, R) = fn v => value S (R v);
	
fun upd (v, t) S = comp (fn w => if w = v then t else Var w, S);
	
exception non_unifiable and occurs_check and length;
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

fun 
  subst(t,v) (Var w)               = if v=w then t else (Var w) |
  subst(t,v) (Fun (f,args))        = Fun (f, map (subst(t,v)) args);

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
(*End Substitution and Unification Implementation*)
