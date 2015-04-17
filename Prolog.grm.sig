signature Prolog_TOKENS =
sig
type ('a,'b) token
type svalue
val VAR: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val UNDER:  'a * 'a -> (svalue,'a) token
val LBRCKT:  'a * 'a -> (svalue,'a) token
val RBRCKT:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val QUEST:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val TS:  'a * 'a -> (svalue,'a) token
val CUT:  'a * 'a -> (svalue,'a) token
val FAIL:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Prolog_LRVALS=
sig
structure Tokens : Prolog_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
