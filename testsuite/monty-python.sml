(* The following reasoning was used by Sir Bedevere in "Monty Python	*)
(* and the Holy Grail" to prove that the Girl is a Witch.		*)

Init();

(*	Assert facts	*)
Prolog (parse "witch(X) :- burns(X), woman(X).");
Prolog (parse "woman(girl).");
Prolog (parse "burns(X) :- isMadeOfWood(X).");
Prolog (parse "isMadeOfWood(X) :- floats(X).");
Prolog (parse "floats(duck).");
Prolog (parse "floats(Y) :- floats(X), sameWeight(X, Y).");
Prolog (parse "sameWeight(duck, girl).");

(* Now the question is whether the Girl is a Witch and, hence, whether	*)
(* to burn her.  The query in Prolog:					*)

Prolog (parse "witch(girl)?");

(* Note: without the cut it will enter an endless loop and search for	*)
(* further solutions after reporting the first solution.		*)
