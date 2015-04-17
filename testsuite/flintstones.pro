(* The Family Tree of the Flintstones -- well, sort of. *)

prolog();
  father(fred, rocky).
  father(fred, dino).
  father(fred, pebbles).
  father(barney, bambam).
  father(bambam, scooby).
  father(bambam, shaggy).
  father(rocky, dafney).
  mother(wilma, pebbles).
  mother(wilma, dino).
  mother(wilma, rocky).
  mother(betty, bambam).
  mother(pebbles, shaggy).
  mother(pebbles, scooby).
  mother(wednesday, dafney).
  male(fred).
  male(barney).
  male(bambam).
  male(rocky).
  male(dino).
  male(shaggy).
  male(scooby).
  female(wilma).
  female(betty).
  female(pebbles).
  female(wednesday).
  female(dafney).
  parent(X,Y) :- father(X,Y).
  parent(X,Y) :- mother(X,Y).
  sibling(X,Y) :- mother(Z,X), mother(Z,Y).
  aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
  uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
  cousin(X,Y) :- mother(Z,X), aunt(Z,Y).
  cousin(X,Y) :- father(Z,X), uncle(Z,Y).
  ances(X,Y) :- parent(X,Y).
  ances(X,Y) :- parent(X,Z), ances(Z,Y).
  parent(fred, pebbles)?
  parent(fred, X)?
  parent(X, bambam)?
  parent(shaggy, X)?
  sibling(pebbles, X)?
  sibling(X, pebbles)?
  aunt(pebbles, X)?
  aunt(X, pebbles)?
  uncle(dino, scooby)?
  uncle(pebbles, X)?
  cousin(scooby, X)?
  cousin(dafney, X)?
  male(X), cousin(X, Y)?
  ances(fred, X)?
  ances(X, scooby)?

