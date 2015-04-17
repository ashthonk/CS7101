(***************************************************************************
 *
 *  ML-LEX Scanner for Prolog
 *
 ***************************************************************************)

type svalue = Tokens.svalue;			(* required *)
type pos = int;					(* required *)
type ('a, 'b) token = ('a, 'b) Tokens.token;	(* required *)
type lexresult = (svalue, pos) token;		(* required *)

val pos = ref 1;				(* required *)
fun eof () = Tokens.EOF (!pos, !pos);		(* required *)

%%
%header (functor PrologLex (structure Tokens: Prolog_TOKENS));
alpha	= [A-Za-z];
digit	= [0-9];
ws	= [\ \t];
id	= [a-z]({alpha}|"_"|{digit})*;
var	= [A-Z]({alpha}|"_"|{digit})*;
cmt1	= "%".*;
cmt2	= "/*".*"*/";
%%
\n		=> (pos := !pos + 1; lex());
{ws}+		=> (lex());
{cmt1}		=> (lex());
{cmt2}		=> (lex());
"fail"		=> (Tokens.FAIL (!pos, !pos));
"!"		=> (Tokens.CUT (!pos, !pos));
"("		=> (Tokens.LPAREN (!pos, !pos));
")"		=> (Tokens.RPAREN (!pos, !pos));
"["		=> (Tokens.LBRCKT (!pos, !pos));
"]"		=> (Tokens.RBRCKT (!pos, !pos));
"|"             => (Tokens.BAR  (!pos, !pos));
","		=> (Tokens.COMMA (!pos, !pos));
"?"		=> (Tokens.QUEST (!pos, !pos));
":-"		=> (Tokens.TS (!pos, !pos));
"."		=> (Tokens.DOT (!pos, !pos));
"_"		=> (Tokens.UNDER (!pos, !pos));
{id}		=> (Tokens.ID (yytext, !pos,!pos));
{var}		=> (Tokens.VAR (yytext, !pos,!pos));
.		=> (print
                      ("PrologLex: ignoring bad character " ^ yytext ^
		       " on line " ^ Int.toString (!pos) ^ "\n");
                     lex()
                   );
