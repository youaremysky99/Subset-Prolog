father(john, mary).
father(john, tom).
father(kevin, john).
mother(eva, mary).
mother(eva, tom).
mother(cristina, john).
mother(cristina, kelly).
mother(kelly, alloy).
male(john).
male(kevin).
male(tom).
female(mary).
female(eva).
female(cristina).
female(kelly).
female(alloy).
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
daughter(X,Y) :- female(X),parent(Y,X).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).
append(nil, Y, Y).
append(cons(X, Xs), Y, cons(X, Rs)) :- append(Xs, Y, Rs).