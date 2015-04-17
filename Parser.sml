(***************************************************************************
 *
 * Parse Functions
 *
 ***************************************************************************)

structure PrologLrVals = PrologLrValsFun (
  structure Token = LrParser.Token
  structure Syntax = Syntax);

structure PrologLex = PrologLex (structure Tokens = PrologLrVals.Tokens);

structure PrologParser = Join (
  structure LrParser = LrParser
  structure ParserData = PrologLrVals.ParserData
  structure Lex = PrologLex);


local
  fun error (errmsg, line, _) =
    print ("Parse error.  Line " ^ (Int.toString line) ^ ": " ^ errmsg ^ "\n")

  fun parse' file =
    let
      fun reader _ = TextIO.input file
      val lexer = PrologParser.makeLexer reader
      val (tree, _) = PrologParser.parse (15, lexer, error, ())
    in
      TextIO.closeIn file;
      tree
    end

in
  fun parse string =
    parse' (TextIO.openString string)

  fun parseFile filename = 
    parse' (TextIO.openIn filename)
end
