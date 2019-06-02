%% trace,string_codes("a.\nb(C,D).\nef(G):-(h(I)->true;true),!.",A),phrase(file(B),A),write(B).

%% [[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],:-,[[[[h,*,*],[i]],->,true,or,true],!]]]

use_module(library(pio)).
use_module(library(dcg/basics)).

:- include('../la_strings.pl').

p2lpconverter :-
	File1="test1.pl",
	readfile(File1,"test1.pl file read error.").
	
readfile(List1,Error) :-
	phrase_from_file_s(string(List6), List1),
	(phrase(file(List3),List6)->true;(writeln(Error),abort)),
	writeln(List3)	.

string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

file([L|Ls]) --> predicate(L),newlines1(_),
%%{writeln(L)}, %%***
file(Ls), !. 
file([L]) --> predicate(L),newlines1(_),
%%{writeln(L)},
!.

%%predicate([]) --> newlines1(_).
predicate(A) -->
		name1(Word11), 
		".", {A=[[Word11,"*","*"]]
		}.
predicate(A) -->
		name1(Word11), 
		"(",varnames(Varnames),")",
		".", {A=[[Word11,"*","*"],Varnames]
		}.
predicate(A) -->
		name1(Word11),
		"(",varnames(Varnames),")",
		spaces1(_),":-",newlines1(_),
		lines(L), ".",
		{A=[[Word11,"*","*"],Varnames,(:-),L]
		}.
		
/**name1([L3|Xs]) --> [X], {string_codes(L2,[X]),(char_type(X,alnum)->true;L2="_"),downcase_atom(L2,L3)}, name1(Xs), !.
name1([]) --> [].
**/

name1(X1) --> name10(X2), {atom_string(X2,X1)}.

name10(XXs) --> [X], 
	{char_code(Ch1,X),(char_type(X,alnum)->true;(Ch1='_'->true;
	Ch1='!')),
	atom_string(CA,Ch1),downcase_atom(CA,CA2)},
	name10(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 
name10(XXs) --> [X], 
	{char_code(Ch1,X),(char_type(X,alnum)->true;(Ch1='_'->true;
	Ch1='!')),
	atom_string(CA,Ch1),downcase_atom(CA,XXs)}, !. 
%%name10('') --> [].

name2(X1) --> name20(X2), {atom_string(X2,X1)}.

name20(XXs) --> [X], 
	{char_code(Ch1,X),%%char_type(X,alnum)->true;
	(Ch1='+'->true;(Ch1='-'->true;(Ch1='*'->true;
	(Ch1='/'->true;(Ch1='<'->true;(Ch1='>'->true;
	(Ch1='='))))))),atom_string(CA,Ch1),
	downcase_atom(CA,CA2)},
	name20(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 
name20(XXs) --> [X], 
	{char_code(Ch1,X),%%(char_type(X,alnum)->true;
	(Ch1='+'->true;(Ch1='-'->true;(Ch1='*'->true;
	(Ch1='/'->true;(Ch1='<'->true;(Ch1='>'->true;
	(Ch1='='))))))),
	atom_string(CA,Ch1),downcase_atom(CA,XXs)}, !. 
%%name20('') --> [].

/**
name2([L3|Xs]) --> [X], {(char_type(X,alnum)->true;
	char_type(X,punct)),string_codes(L2,[X]),downcase_atom(L2,L3)}, name2(Xs), !.
name2([]) --> [].**/

%%spaces1(_) --> [X], {char_type(X,space)},!.
spaces1([X|Xs]) --> [X], {char_type(X,space)}, spaces1(Xs), !.
spaces1([]) --> [].

varnames([L1|Ls]) --> name1(L1),",", %%{writeln(L)}, %%***
varnames(Ls), !. 
varnames([L4]) --> name1(L1), {string_codes(L2,L1),downcase_atom(L2,L3),atom_string(L3,L4)},!.


newlines1([X|Xs]) --> [X], {char_type(X,newline)}, newlines1(Xs), !.
%%newlines1([X]) --> [X], {char_type(X,newline)},!.
newlines1([]) --> [],!.

/**
was comments
newlines1([]) --> "%", comments(_), "\n".
newlines1([]) --> "/","*", commentsa(_), "*","/".
newlines1([]) --> newlines1(_).

comments([L|Ls]) --> comments2(L),
%%{writeln(L)}, %%***
comments(Ls), !. 
comments([L]) --> comments2(L), 
%%{writeln(L)},
!.

comments2(_) --> spaces1(_),name1(_).%%[X], {string_codes(X1,[X]), not(X1="\n")}.


commentsa([L|Ls]) --> comments3(L),
%%{writeln(L)}, %%***
commentsa(Ls), !. 
commentsa([L]) --> comments3(L), 
%%{writeln(L)},
!.

comments3(_) --> spaces1(_),name1(_).%%[X], [Y], {string_codes(X1,[X]),
	%%string_codes(Y1,[Y]), not((X1="*",Y1="/"))}.

**/

lines([L|Ls]) --> line(L),",",newlines1(_),
%%{writeln(L)}, %%***
lines(Ls), !. 
lines([L]) --> line(L), 
%%{writeln(L)},
!.

line(A) --> %%spaces1(_), 
		name1(Word11), %% name(A,B,C).
		"(",varnames(Varnames),")",
		{A=[[Word11,"*","*"],Varnames]},!.
line(A) --> %%spaces1(_), 
		name1(Word10), spaces1(_), %% A = B*Y
		(name1(Word21)|name2(Word21)), spaces1(_), 
		name1(Word11), 	
		name2(Word12), name1(Word13), 	
		{string_concat(Word11,Word12,B),
		string_concat(B,Word13,C),
		A=[Word10,Word21,C]},!.
line(A) --> %%spaces1(_), 
		name1(Word10), spaces1(_), %% A is B
		name2(Word21), spaces1(_), name1(Word11),
		{A=[Word10,Word21,Word11]},!.
line(Word1) -->
		"(",line(Word2),")",{Word1=[Word2]},!.
line(Word1) -->
		"(",line(Word2),"->",line(Word3),";",line(Word4),")",
		{Word1=[Word2,->,Word3,or,Word4]},!.
line(Word1) -->
		"(",line(Word2),"->",line(Word3),")",
		{Word1=[Word2,->,Word3]},!.
line(Word1) -->
		"(",line(Word2),";",line(Word3),")",
		{Word1=[Word2,or,Word3]},!.
line(Word) --> %%spaces1(_), 
		name1(Word), 
		{(Word="true"->true;Word="!")},!.
line(Word1) -->
		"(",lines(Word2),")",
		{Word1=[Word2]},!.
%%a(_) -->",".
%%line([]) --> newlines1(_),!.


%% **** Pretty Print

/**

?- pp0([[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],(:-),[[[[h,*,*],[i]],->,true,or,true],!]]]).
[
[[a,*,*]],
[[b,*,*],[c,d]],
[[ef,*,*],[g],(:-),
	[[[h,*,*],[i]],->,true,or,true],
	!]
]

**/

concat_list(A,[],A):-!.
concat_list(A,List,B) :-
	List=[Item|Items],
	string_concat(A,Item,C),
	concat_list(C,Items,B).


pp0(List) :-
	writeln("["),
	pp1(List),
	writeln("]").

pp1([]):-!.
pp1(List1) :-
	List1=[List2|Lists3],
	(((List2=[[_Name,*,*]]->true;List2=[[_Name,*,*],
		_Variables]),
	write(List2),writeln(","))->true;
	(List2=[[Name,*,*],Variables1,(:-),Body]->
	(term_to_atom(Variables1,Variables2),
	concat_list("[[",[Name,",*,*],",Variables2,",(:-),"],String),
	writeln(String),pp2(Body),writeln("]")))),
	pp1(Lists3).
pp2([]):-!.
pp2(List1) :-
	List1=[List2],
	write("\t"),write(List2),!.
pp2(List1) :-
	List1=[List2|Lists3],
	write("\t"),write(List2),writeln(","),
	pp2(Lists3).
	