signature TERMS = 
    sig
      eqtype Operator and Variable and Term;
      datatype TT = TO of Operator * TT list | TV of Variable;
      val convert : Term -> TT;
      val unconvert : TT -> Term;
    end;

signature UNIFICATION = 
   sig
     type Term and Substitution;
     exception non_unifiable and occurs_check;
     val unify : (Term * Term * Substitution) -> Substitution;
     val id : Substitution;
     val value : Substitution -> Term -> Term;
   end;

functor Unification (Terms: TERMS) : UNIFICATION =
   struct
	type Term = Terms.Term;
	type Substitution = Terms.Variable -> Terms.Term;
	
	val id = (Terms.unconvert o Terms.TV); (* What does this eman?? *)

	fun upd (v: Terms.Variable, t) (S) (v1:Terms.Variable) =
	  if v=v1 then t else (S v1);
	
	fun
	  top S (Terms.TV v) = S v   |
	  top S (x) 	     = x     ;

	fun 
	  value' S (Terms.TV v)      = S v     |
	  value' S (Terms.TO(O,args)) = Terms.TO(O, map (value' S) args);

	fun value (S: Substitution) t : Terms.Term =
	 Terms.unconvert (value' (Terms.convert o S) (Terms.convert t)); (*This*)
	exception non_unifiable and occurs_check and length;
	fun
	  pairup (nil, nil)     = nil       |
	  pairup (a::b, c::d)   = (a,c)::(pairup (b,d)) |
          pairup (_)            = raise length;

	fun
	  occurs v (Terms.TV w)          = (v = w)    |
	  occurs v (Terms.TO (O, args))  = List.exists (occurs v) args;

	fun
	  subst(t,v) (Terms.TV w)        = if v=w then t else (Terms.TV w) |
	  subst(t,v) (Terms.TO (O,args)) = Terms.TO(O,map (subst(t,v)) args);
       fun unify' ((t1,t2),S) =
          let
            val t1' = top S t1 and t2' = top S t2;
          in
            case (t1', t2') of
                (Terms.TV v,Terms.TV w) => if (v=w) then S else upd(v,t2')S |
                (Terms.TV v, _)         =>
                  if occurs v t2 then raise occurs_check else upd(v,t2')S |
                (_, Terms.TV w)         =>
                  if occurs w t1 then raise occurs_check else upd(w,t1')S |
                (Terms.TO (o1, tlist1), Terms.TO (o2, tlist2)) =>
                  if o1=o2
                    then foldr unify' S (pairup (tlist1, tlist2))
                    else raise non_unifiable
        end;

        fun unify (t1,t2,S) =
          let
            val t = (Terms.convert t1, Terms.convert t2)
          in
            Terms.unconvert o unify'(t, Terms.convert o S) (*This*)
        end;
   end;

