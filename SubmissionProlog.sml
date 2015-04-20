(*
Jace Courville
CS710107
File: Prolog.sml

Notes:
After a very arduous time trying to get this going, I have gotten as much work on this project as I possibly could, given the amount of time it took me to functionally understand both ML and Prolog as languages. I will wholeheartedly admit that this implementation is a bit verbose and could be optimized a fair bit, and in truth I had started work on a more optimized version that may have fixed a major bug I was encountering.

As far as I can tell, my implementation will bug out in situations when you baacktrack in a rule. The specific case I was trying to get working was from the flintstones.sml file - with uncle(dino,scooby). It seems that I would hit an infinite loop when trying to do the final climb out of the rule to present the answer. Many other test cases that I had tried out functioned quite well.


*)
val termlist: ((Term * Term) list) ref = ref [];
fun AddTerm(t1,termlist) = (termlist := !termlist @ [t1]);
fun CleanTL(tl) = (tl := []);

(*This is the Unification and Substitution steps*)
type Substitution = string * int  -> Term;

val empty : Substitution = fn x => Var x;
fun addone (Var(a:string,b:int)) = (Var(a,b+1));
fun 
    value S (Var v) = (S v) |
    value S (Fun(f,args)) = (Fun (f,map (value S) args)); 
fun comp (S, R) = (( fn v => value S (R v)));
       
fun upd (v, t) S = (comp (fn w => if w = v then t else Var w, S));
fun upd''((Var v)::at,bh::bt) = 
       ( 
	let val V = empty
	    val W = comp(V, (upd(v,bh)V)) 
	in
	if not(at = []) then comp(W,upd''(at,bt)) else W
        end )
 | upd''((Fun(v,args))::at,bh::bt) =
       (
	if not(at=[]) then upd''(at,bt) else empty
        );
 
fun upd'(t1 as(Fun(f, args1)),t2 as(Fun(y,args2))) = ( 
					if (t1=t2) then empty else if not(args1 = []) then upd''(args1,args2) else empty);
fun LevelUp(Var(x,y),level) = (Var(x,level)) 
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
fun unify (t1,t2,level,nil) =
			(
			let val R = (unify' ((t1,t2),empty)
			    handle occurs_check => raise non_unifiable
				|length         => raise non_unifiable)
			in
			 comp(R,upd'(t1,LevelUp((t2),level)))
			end);
 
(*Search and P1-P3 will navigate the list of terms and clean out any duplicate terms prior to the final output. This can probably be done much more optimally, but I will re-iterate my novice status of functional programming languages here)*)
fun Search(st : Term, []) = false | Search(st : Term, (tlh1,tlh2)::tlt : (Term * Term) list) = if (st=tlh1) then true else Search(st,tlt);
fun P3(xh::xt : Term list,zh::zt : Term list) = 
		if xt = [] 
		then (if not(Search(xh,!termlist)) 
		      then if not(xh=zh) 
                           then (AddTerm((xh,zh),termlist)) 
                           else () 
                      else ()) 
               else (if not(Search(xh,!termlist)) 
                     then (if not(xh=zh) 
                           then (AddTerm((xh,zh),termlist))
                           else (); 
                           P3(xt,zt)) 
                     else (P3(xt,zt))); 
fun P2(Fun(x,xlist),Fun(z,zlist)) = P3(xlist,zlist);
fun P1(yh, S) = (P2(yh, (value S yh)));

fun yheadcheck(Fun(f,_),Fun(g,_)) = if (f=g) then true else false;
fun checker(term,(yh::yt)) = (if (term = yh) then true else false);	
(*This is the solver*)	  

fun Solver(y as (yhead::ytail), ((Headed(hch,[]))::dbtail), database, querylist, counter, level) =
	(
	let val S = unify(yhead,hch,level,nil)
		     handle non_unifiable => (if (dbtail = []) then (raise non_solvable)
								else (
								      Solver(y,dbtail,database,querylist,counter,level)
									    handle non_solvable => raise non_solvable
										 ))
	in
		if (not(ytail=[])) (*We still have more to do*)
		then ( 
			if checker(yhead,ytail) then (S) else
			comp(S, Solver((map (value S) ytail), database, database, querylist, counter, level)
				handle non_solvable => if (dbtail = []) then ( 
									      raise non_solvable)
									else (									       
									      comp (S, Solver(y,dbtail,database,querylist,counter,level)
									            handle non_solvable => raise non_solvable)
										     ))
		     )
		else (S)
	end
	)
 |  Solver(y as (yhead::ytail), ((Headed(hch,hct))::dbtail), database, querylist, counter, level) = 
	(
	let val S = unify(hch,yhead,level,nil)
		    handle non_unifiable => (if(dbtail = []) then (raise non_solvable)  (*Current query in list doesn't work. No matter. Next in line! *)
							     else (Solver(y,dbtail,database,querylist,counter,level)))
	in	
		if (yheadcheck(yhead,hch))
		then(
		let val Y =  
		comp(S,Solver((map (value S) hct),database,database,querylist,counter,(level+1))
		 handle non_solvable => if (dbtail = []) then ( (* There's actually soemthing wrong with this rule. This implies that whatever we mapped didn't go through, and we have nothing left to try *)
							 raise non_solvable) 
							 else ((* This hapens when some subrule fails, and we need to try a different subrule *)
							      (let val Z = comp (S,(Solver((map (value S) hct),dbtail,database,querylist,counter,level)handle non_solvable => raise non_solvable))
							       in  
								  upd'(yhead,(value Z(value S hch))) end)
							  ))
		in
		 upd'(yhead,(value Y (value S hch)))
		end
		)
		else (*We're done? May fail if more than one term in the rule*)S
	end
	);
	


(*End Solver*)	
fun Print ((yhead::ytail),S) = 
	(
	  if not(ytail = nil) then (P1(yhead,S); Print(ytail,S)) else P1(yhead,S) 
	);
(*OutQuery - Handles the initial call to the solver. Handles a few special cases as well*)
fun OutQuery (y as (yhead::ytail) : Term list, database as (dbh::dbt) : HornClause list) =
	(
	 let val count = 0
	 in 
	 Print(y, Solver(y,database,database,y,count,1)
		handle  non_solvable => if dbt = [] then raise non_solvable else Solver(y,dbt,database,y,count,0))
         end;
     OutSol(!termlist) 
 	)
 |   OutQuery (_,[]) = OutLine("Empty Database");
(*End OutQuery*)

fun Prolog (x as (Headed (Var _, _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
   |Prolog (x as (Headless (Var _ :: _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (Headed (Fun ("init", nil),nil)) = InitDB ()
  | Prolog (x as (Headed _)) = Assert x
  | Prolog (x as Headless y) =
    (
     CleanTL(termlist); 
     OutLine ("query:  " ^ PrintClause x);
     OutQuery (y, !db)
      handle non_solvable => OutLine ("No")
   );


