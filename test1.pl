:- use_module(library(date)).

:-include('luciancicd.pl').
:-include('../Prolog-to-List-Prolog/p2lpconverter.pl').

use_module(library(pio)).
use_module(library(dcg/basics)).


:-dynamic keep_comments/1.

a:-a.
a(/*1*/%2

A,B).
a(/*1*/B,C)/*2*/:-d(E).
a(/*1*/B,C)/*2*/->d(E).
a/*2*/:-/*3*/d(E).
a/*2*/->/*3*/d(E).
a->a.
a/*2*/-->/*3*/d(E).
a:-b([A,B],1,[C]).
a./*1*/
b:-c.
a:-b,
/*1*/
c,
%2
d.
a(A):-[]=[[]].
a(A):-([]=[[]]).
a(A):-([1]=A).
a(A):-(B=A).
a(A):-([1]=[1]).
a([A,B,C]).
a([A,B]).
a([A|C]).
a([[A,B]]).
a(A,[]).
a(A,[A]).
a([[A]|C]).
a(A,[[A]|C]).
a(A,[[A,B]|[C,D]]).
ba(12).
ba(12,1).
a(1.1).
a("dsf").
a('dd').
a(A):-findall(A,hello(A),B).
a(A):-findall(A,hello(A),B),!.
a(A):-findall(A,(hello(A),hello(A)),B).
a([A]):-A is 1+1.
ef(G):-(h(I)->true;true).
ef(G):-(h(I)->true;true),!.
ef(G):-(h(I)->true).
ef(G):-(h(I)->true),!.
compound21(T,U)->item(I).
compound21(T,U)->item(I),!.
a([A]).
a(A).
compound21(T,U)->{wrap(I,Itemname1)}.
compound21(T,U)->{wrap(I,Itemname1),append(T,Itemname1,V)}.
compound21(T,U)->item(I),lookahead("]"),{wrap(I,Itemname1),append(T,Itemname1,V)},compound212(V,U).
a(A):-findall([A,C],hello(A,C),B).
