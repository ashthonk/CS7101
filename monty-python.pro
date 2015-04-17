(* The following reasoning was used by Sir Bedevere in "Monty Python	*)
(* and the Holy Grail" to prove that the Girl is a Witch.		*)

prolog();
  witch(X) :- burns(X), woman(X).
  woman(girl).
  burns(X) :- isMadeOfWood(X).
  isMadeOfWood(X) :- floats(X).
  floats(duck).
  floats(Y) :- floats(X), !, sameWeight(X, Y).
  sameWeight(duck, girl).
  witch(girl)?

