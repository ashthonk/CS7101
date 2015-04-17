(* Top-level loop of the Prolog interpreter. *)

structure TopLevel =
struct

  local
    fun
      TopLevel () = (
        print "PROLOG> ";
        case TextIO.inputLine TextIO.stdIn of
  	    NONE => ()
	  | SOME "\n" => TopLevel ()
	  | SOME inp =>
	    let
	      val code = parse inp
	    in
	      if code = (Headed (Fun ("end_of_file", []), []))
	      then ()
	      else (
		Prolog code;
		TopLevel ()
	      )
	    end handle PrologParser.ParseError => TopLevel ()
    )
  in
    fun prolog () =
      (db := [];
       TopLevel ()
      )
  end

  fun main (name, args) =
    (prolog ();
     OS.Process.success)
end
