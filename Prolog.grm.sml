
functor PrologLrValsFun (structure Token : TOKEN
                                  structure Syntax : SYNTAX ) : Prolog_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(***************************************************************************
 *
 * ML-YACC Parser for Prolog
 *
 ***************************************************************************)

local
  val counter = ref 0;
  fun next () = (counter := !counter + 1; !counter)
in
  fun temp () = "$x" ^ (Int.toString (next ()))
end;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\003\000\009\000\011\000\017\000\012\000\008\000\013\000\007\000\
\\014\000\006\000\015\000\005\000\000\000\
\\001\000\003\000\009\000\012\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\000\000\
\\001\000\005\000\023\000\006\000\011\000\000\000\
\\001\000\006\000\011\000\007\000\010\000\000\000\
\\001\000\006\000\011\000\008\000\024\000\000\000\
\\001\000\006\000\011\000\010\000\022\000\011\000\021\000\000\000\
\\001\000\011\000\026\000\000\000\
\\028\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\\031\000\004\000\013\000\005\000\012\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\009\000\014\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\"
val actionRowNumbers =
"\002\000\004\000\012\000\015\000\
\\017\000\014\000\001\000\016\000\
\\010\000\002\000\008\000\002\000\
\\002\000\006\000\011\000\019\000\
\\013\000\003\000\005\000\020\000\
\\002\000\009\000\018\000\007\000\
\\021\000\000\000"
val gotoT =
"\
\\001\000\025\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\014\000\003\000\013\000\000\000\
\\000\000\
\\000\000\
\\002\000\016\000\000\000\
\\000\000\
\\002\000\014\000\003\000\017\000\000\000\
\\002\000\014\000\003\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 26
val numrules = 13
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | VAR of  (string)
 | ID of  (string) | termlist of  (Syntax.Term list)
 | term of  (Syntax.Term) | clause of  (Syntax.HornClause)
end
type svalue = MlyValue.svalue
type result = Syntax.HornClause
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "FAIL"
  | (T 2) => "CUT"
  | (T 3) => "TS"
  | (T 4) => "DOT"
  | (T 5) => "COMMA"
  | (T 6) => "QUEST"
  | (T 7) => "RPAREN"
  | (T 8) => "LPAREN"
  | (T 9) => "BAR"
  | (T 10) => "RBRCKT"
  | (T 11) => "LBRCKT"
  | (T 12) => "UNDER"
  | (T 13) => "ID"
  | (T 14) => "VAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.term term, 
term1left, _)) :: rest671)) => let val  result = MlyValue.clause (
Syntax.Headed (term, []))
 in ( LrTable.NT 0, ( result, term1left, DOT1right), rest671)
end
|  ( 1, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.termlist 
termlist, _, _)) :: _ :: ( _, ( MlyValue.term term, term1left, _)) :: 
rest671)) => let val  result = MlyValue.clause (
Syntax.Headed (term, termlist))
 in ( LrTable.NT 0, ( result, term1left, DOT1right), rest671)
end
|  ( 2, ( ( _, ( _, _, QUEST1right)) :: ( _, ( MlyValue.termlist 
termlist, termlist1left, _)) :: rest671)) => let val  result = 
MlyValue.clause (Syntax.Headless (termlist))
 in ( LrTable.NT 0, ( result, termlist1left, QUEST1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.term term, term1left, term1right)) :: 
rest671)) => let val  result = MlyValue.termlist ([term])
 in ( LrTable.NT 2, ( result, term1left, term1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.term term, _, term1right)) :: _ :: ( _, ( 
MlyValue.termlist termlist, termlist1left, _)) :: rest671)) => let
 val  result = MlyValue.termlist (termlist @ [term])
 in ( LrTable.NT 2, ( result, termlist1left, term1right), rest671)
end
|  ( 5, ( ( _, ( _, UNDER1left, UNDER1right)) :: rest671)) => let val 
 result = MlyValue.term (Syntax.Var (Header.temp (), 0))
 in ( LrTable.NT 1, ( result, UNDER1left, UNDER1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.VAR VAR, VAR1left, VAR1right)) :: rest671))
 => let val  result = MlyValue.term (Syntax.Var (VAR, 0))
 in ( LrTable.NT 1, ( result, VAR1left, VAR1right), rest671)
end
|  ( 7, ( ( _, ( _, CUT1left, CUT1right)) :: rest671)) => let val  
result = MlyValue.term (Syntax.Fun ("!", []))
 in ( LrTable.NT 1, ( result, CUT1left, CUT1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.term (Syntax.Fun (ID, []))
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.termlist 
termlist, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.term (
Syntax.Fun (ID, termlist))
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRCKT1right)) :: ( _, ( _, LBRCKT1left, _))
 :: rest671)) => let val  result = MlyValue.term (
Syntax.Fun ("$Nil", []))
 in ( LrTable.NT 1, ( result, LBRCKT1left, RBRCKT1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RBRCKT1right)) :: ( _, ( MlyValue.termlist 
termlist, _, _)) :: ( _, ( _, LBRCKT1left, _)) :: rest671)) => let
 val  result = MlyValue.term (
List.foldr
					     (fn (x, y) => 
						 Syntax.Fun ("$Cons", [x, y])
					     )
					     (Syntax.Fun ("$Nil", []))
					     termlist
)
 in ( LrTable.NT 1, ( result, LBRCKT1left, RBRCKT1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RBRCKT1right)) :: ( _, ( MlyValue.term term, _
, _)) :: _ :: ( _, ( MlyValue.termlist termlist, _, _)) :: ( _, ( _, 
LBRCKT1left, _)) :: rest671)) => let val  result = MlyValue.term (
List.foldr
					     (fn (x, y) =>
						 Syntax.Fun ("$Cons", [x, y])
					     )
					     term
					     termlist
)
 in ( LrTable.NT 1, ( result, LBRCKT1left, RBRCKT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.clause x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Prolog_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FAIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun CUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun QUEST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRCKT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRCKT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.ID i,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VAR i,p1,p2))
end
end
