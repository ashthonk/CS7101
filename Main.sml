(*  Gather all the files necessary to make the Prolog parser
    and other auxiliary routines.  *)

val PROLOG_FILE = "Prolog.sml";

val EXPORT_ML	= false;
val EXEC_FILE   = "pml";

(* For Linux: *)
val MLTOOL_DIR  = "/usr/lib/smlnj/lib";
val MLYACC_DIR  = "/usr/lib/smlnj/ml-yacc";

(* For Windows: *)
(* val MLTOOL_DIR  = "C:/Program Files/SMLNJ/lib"; *)
(* val MLYACC_DIR  = "C:/Program Files/SMLNJ/src/ml-yacc/lib"; *)

(* Don't change anything below. *)

(* For some reason autoloading a library and make aren't working right. -gb *)
(* CM.autoload (MLTOOL_DIR ^ "/SMLNJ-ML-YACC-LIB"); *)
(* CM.make "sources.cm"; *)

use (MLYACC_DIR ^ "/base.sig");
use (MLYACC_DIR ^ "/join.sml");
use (MLYACC_DIR ^ "/lrtable.sml");
use (MLYACC_DIR ^ "/stream.sml");
use (MLYACC_DIR ^ "/parser2.sml");

use "Syntax.sig";
use "Syntax.sml";
use "Prolog.grm.sig";
use "Prolog.grm.sml";
use "Prolog.lex.sml";
use "Parser.sml";

open Syntax;

use PROLOG_FILE;
use "TopLevel.sml";

if EXPORT_ML then SMLofNJ.exportFn (".heap/" ^ EXEC_FILE, TopLevel.main)
else ();

open TopLevel;

(* Type 'prolog ();' to run your Prolog interpreter. *)
(* Type ^C to exit the Prolog interpreter. *)
(* Type ^D to exit SML. *)
