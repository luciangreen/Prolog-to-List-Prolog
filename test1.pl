a([A,B]).
a([A|C]).
a([[A,B]]).
a(A,[]).
a(A,[A]).
a([[A]|C]).
a(A,[[A]|C]).
a(A,[[A,B]|[C,D]]).
ba(12).
a(1.1).
a("dsf").
a('dd').
a(A):-findall(A,(hello(A)),B).
a([A]):-A is 1+1.
ef(G):-(h(I)->true;true),!.
a([A]).
a(A).
