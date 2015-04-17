(*  Abstract syntax of Prolog.  *)

signature SYNTAX =
    sig
        (*
          Type definition for terms.
          We don't distinguish between function symbols and predicate
          symbols.  Variables are the same if they have the same name
          and same instance number.
        *)
        datatype Term =
          Fun of string * Term list |
          Var of string * int       ;

        (*
          Type definition for Horn clauses.
          Headed clauses are used for facts and rules in the database.
          Headless clauses are used for queries.
        *)
        datatype HornClause =
          Headed of Term * Term list   |
          Headless of Term list        ;

        (*
          The data base is a variable containing a list of clauses.
        *)
        val db: (HornClause list) ref;
        val PrintTerm: Term -> string;         (* Un-parse Term to string   *)
        val PrintClause: HornClause -> string; (* Un-parse HornClause       *)
        val PrintList: Term list -> string;    (* Un-parse Term list to string*)
        val PrintDB: HornClause list -> string;(* Un-parse DB to string     *)
	val OutLine: string -> unit;           (* Output a string on a line *)
        val OutSol: (Term * Term) list -> unit;(* Output one solution       *)
        val InitDB: unit -> unit;              (* Erase the database        *)
	val Assert: HornClause -> unit;        (* Add to the database       *)
(*	val OutQuery: Term list * HornClause list -> unit;  (* Report answer      *) *)
    end;
