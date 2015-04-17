(* The Family Tree of the Flintstones -- well, sort of. *)

Prolog (parse "init.");

(*	Assert facts	*)
Prolog (parse "father(fred, rocky).");
Prolog (parse "father(fred, dino).");
Prolog (parse "father(fred, pebbles).");
Prolog (parse "father(barney, bambam).");
Prolog (parse "father(bambam, scooby).");
Prolog (parse "father(bambam, shaggy).");
Prolog (parse "father(rocky, dafney).");
Prolog (parse "mother(wilma, pebbles).");
Prolog (parse "mother(wilma, dino).");
Prolog (parse "mother(wilma, rocky).");
Prolog (parse "mother(betty, bambam).");
Prolog (parse "mother(pebbles, shaggy).");
Prolog (parse "mother(pebbles, scooby).");
Prolog (parse "mother(wednesday, dafney).");
Prolog (parse "male(fred).");
Prolog (parse "male(barney).");
Prolog (parse "male(bambam).");
Prolog (parse "male(rocky).");
Prolog (parse "male(dino).");
Prolog (parse "male(shaggy).");
Prolog (parse "male(scooby).");
Prolog (parse "female(wilma).");
Prolog (parse "female(betty).");
Prolog (parse "female(pebbles).");
Prolog (parse "female(wednesday).");
Prolog (parse "female(dafney).");


(*  Note that the definitions are not perfect.  For example, you are	*)
(*  sometimes your own sibling, sometimes not.				*)

(*	Assert rules	*)
Prolog (parse "parent(X,Y) :- father(X,Y).");
Prolog (parse "parent(X,Y) :- mother(X,Y).");

Prolog (parse "sibling(X,Y) :- mother(Z,X), mother(Z,Y).");

Prolog (parse "aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).");

Prolog (parse "uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).");

Prolog (parse "cousin(X,Y) :- mother(Z,X), aunt(Z,Y).");
Prolog (parse "cousin(X,Y) :- father(Z,X), uncle(Z,Y).");

Prolog (parse "ances(X,Y) :- parent(X,Y).");
Prolog (parse "ances(X,Y) :- parent(X,Z), ances(Z,Y).");


(*	Queries		*)
Prolog (parse  "parent(fred, pebbles)?");
Prolog (parse  "parent(fred, X)?");
Prolog (parse  "parent(X, bambam)?");
Prolog (parse  "parent(shaggy, X)?");
Prolog (parse  "sibling(pebbles, X)?");
Prolog (parse  "sibling(X, pebbles)?");
Prolog (parse  "aunt(pebbles, X)?");
Prolog (parse  "aunt(X, pebbles)?");
Prolog (parse  "uncle(dino, scooby)?");
Prolog (parse  "uncle(pebbles, X)?");
Prolog (parse  "cousin(scooby, X)?");
Prolog (parse  "cousin(dafney, X)?");
Prolog (parse  "male(X), cousin(X, Y)?");
Prolog (parse  "ances(fred, X)?");
Prolog (parse  "ances(X, scooby)?");
