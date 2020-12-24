:-include('../listprologinterpreter/la_strings.pl').

%% p2lpconverter(S1),pp0(S1,S2),writeln(S2).

/**trace,
string_codes("a.\nb(C,D).\nef('A'):-(h(a)->true;true),!.",A),phrase(file(B),A),write(B).
**/

%% [[[n,a]],[[n,b],[[v,c],[v,d]]],[[n,ef],['A'],:-,[[[n,->],[[[n,h],[a]],[[n,true]],[[n,true]]]],[[n,cut]]]]]


use_module(library(pio)).
use_module(library(dcg/basics)).

:- include('la_strings.pl').

p2lpconverter(List3) :-
	File1="test1.pl",
	readfile(File1,"test1.pl file read error.",List3).
	
readfile(List1,Error,List3) :-
	phrase_from_file_s(string(List6), List1),
	(phrase(file(List3),List6)->true;(writeln(Error),abort)).
	%writeln1(List3)	.

string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

file([L|Ls]) --> predicate(L),newlines1(_),
{writeln1(L)}, %%***
file(Ls), !. 
file([L]) --> predicate(L),newlines1(_),
{writeln1(L)},
!.

%%predicate([]) --> newlines1(_).
predicate(A) -->
		name1(Word11), 
		".", {A=[[n,Word11]]
		}.
predicate(A) -->
		name1(Word11), 
		"(",varnames(Varnames),")",
		".", {A=[[n,Word11],Varnames]
		}.
predicate(A) -->
		name1(Word11),
		"(",varnames(Varnames),")",
		spaces1(_),":-",newlines1(_),
		lines(L), ".",
		{A=[[n,Word11],Varnames,":-",L]
		}.
		
/**name1([L3|Xs]) --> [X], {string_codes(L2,[X]),(char_type(X,alnum)->true;L2="_"),downcase_atom(L2,L3)}, name1(Xs), !.
name1([]) --> [].
**/

name1(X1) --> name10(X11),
	{(string_atom(X12,X11),number_string(X1,X12)->true;
	((%contains_string(X11)->
	string_atom2(X1,X11)%;X11=X1)
	)))}.%%., X2->X1 {atom_string(X2,X1)}.

name1(X1) --> name2(X1).

name10(XXs) --> [X], 
	{char_code(Ch1,X),(char_type(X,alnum)->true;(Ch1='_'->true;(
	Ch1='!'->true;Ch1='.'))),
	atom_string(CA,Ch1),downcase_atom(CA,CA2)},
	name10(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 
name10(XXs) --> [X], 
	{char_code(Ch1,X),(char_type(X,alnum)->true;(Ch1='_'->true;
	Ch1='!')),
	atom_string(CA,Ch1),downcase_atom(CA,XXs)}, !. 
%%name10('') --> [].

name11(X1) --> name101(X11),
	{(string_atom(X12,X11),number_string(X1,X12)->true;
	((%contains_string(X11)->
	string_atom2(X1,X11)%;X11=X1)
	)))}.%%., X2->X1 {atom_string(X2,X1)}.

name101(XXs) --> [X], 
	{char_code(Ch1,X),(char_type(X,alnum)->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	Ch1='!'->true;Ch1='.')))),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	name101(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 
name101(XXs) --> [X], 
	{char_code(Ch1,X),(char_type(X,alnum)->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	Ch1='!'->true;Ch1='.')))),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	%name101(Xs), 
	{atom_concat(CA2,'',XXs)}, !. 

name2(X1) --> name20(X1).%%, {atom_string(X2,X1)}.

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

/**
varnames([L1|Ls]) --> varname1(L1),",", %%{writeln(L)}, %%***
varnames(Ls), !. 
varnames([L1]) --> varname1(L1),
!.
**/

varnames01(L1) --> %{trace},
"[",varnames0(L2),"]", 
{L1 = [L2]},
!. 

varnames01(L1) --> varname1(L2),
	{L1=L2},!.

varnames(L1) --> %{trace},
"[",varnames0(L2),"]", 
{L1 = [L2]},
!. 

varnames(L1) --> %{trace},
varnames0(L1), 
!. 

varnames0(L1) --> varname1(L2),lookahead1,
	{L1=[L2]},!.

varnames0(Ls2) --> varname1(L1),",", %%{writeln(L)}, %%***
	varnames0(Ls), 
	{append([L1],Ls,Ls2)},!. 

varnames0(Ls2) --> varname1(L1),"|", %%{writeln(L)}, %%***
	varnames0([Ls]), 
	{append_list([[L1],"|",Ls],Ls2)},!. 

lookahead1(A,A) :- append(`]`,_,A).
lookahead1(A,A) :- append(`)`,_,A).
%lookahead1(A,A) :- append(`,`,_,A).

varname1([]) --> "[","]". %%{writeln(L)}, %%***
varname1(L4) --> name11(L1), %%{writeln(L)}, %%***
{%%atom_string(L1,L10),string_codes(L2,L10),
((atom_concat(A,_,L1),atom_length(A,1),not(is_upper(A))->L4=L1;(downcase_atom(%%L2
L1,L3),L4=[v,L3])))%%L3A

%%,term_to_atom(L3A,L4)%%,atom_string(L4A,L4)
}.
varname1(L4) --> "(",line(L4),")".
varname1(L1) --> 
"[",varnames0(L2),"]", 
{L1 = L2},!.


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
		{%trace,
		not(Word11=findall)},
		"(",varnames(Varnames),")",
		{A=[[n,Word11],Varnames]},!.
line(A) --> %%spaces1(_), 
		name1(Variable1),{%trace,
		not(Variable1=findall)},
		spaces1(_), %% A = B*Y
		(name1(_Is)|name2(_Equals)), spaces1(_), 
		name1(Variable2), 	
		name2(Operator), name1(Variable3), 	
		{ %% A=B*Y 
		v_if_string_or_atom(Variable2,Variable2a),
		v_if_string_or_atom(Variable3,Variable3a),
		v_if_string_or_atom(Variable1,Variable1a),
		A=[[n,Operator],[Variable2a,Variable3a,Variable1a]]},!.
line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		spaces1(_), %% A is B
		name2(Word21), spaces1(_), name1(Word11),
		{v_if_string_or_atom(Word10,Word10a),
		v_if_string_or_atom(Word11,Word11a),
		A=[[n,Word21],[Word10a,Word11a]]},!.

line(A) --> %%spaces1(_), 
		name1(Word11), %% name(A,B,C)
		{%trace,
		Word11=findall},
		"(",varnames01(Varnames),",","(",lines(A1),")",",",varnames01(Varnames2),")",
		{A=[[n,Word11],[Varnames,A1,Varnames2]]},!.

line(A) --> %%spaces1(_), 
		name1(Word11), %% name(A,B,C)
		{%trace,
		Word11=findall},
		"(",varnames01(Varnames),",",line(A1),",",varnames01(Varnames2),")",
		{A=[[n,Word11],[Varnames,A1,Varnames2]]},!.
		
		
line(Word1) -->
		"(",line(Word2),")",{Word1=[Word2]},!.
line(Word1) -->
		"(",line(Word2),"->",line(Word3),";",line(Word4),")",
		{Word1=[[n,"->"],[Word2,Word3,Word4]]},!.
line(Word1) -->
		"(",line(Word2),"->",line(Word3),")",
		{Word1=[[n,"->"],[Word2,Word3]]},!.
line(Word1) -->
		"(",line(Word2),";",line(Word3),")",
		{Word1=[[n,or],[Word2,Word3]]},!.
line([[n,cut]]) --> %%spaces1(_), 
		name1(Word), 
		{not(Word=findall)},
		{Word=!},!.
line([[n,Word]]) --> %%spaces1(_), 
		name1(Word),{%trace,
		not(Word=findall)}.
%%		{Word=true},!.

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
[
	[[[h,*,*],[i]],->,true,or,true],
	!
]]
]
**/
/**	
concat_list(A,[],A):-!.
concat_list(A,List,B) :-
	List=[Item|Items],
	string_concat(A,Item,C),
	concat_list(C,Items,B).
**/

pp0(List,String2) :-
%trace,
	pp1(List,'',String1),
	concat_list(['[\n',String1],String5),
	%replace(String3,"&","\"",String4),
	%replace(String3,"#","'",String5),
	string_concat(String6,B,String5),string_length(B,2),
	string_concat(String6,'\n]',String2),
	!.
pp1([],S,S):-!.
pp1(List1,S1,S2) :-
	List1=[List2],
	(((List2=[[n,_Name]]->true;List2=[[n,_Name],
		_Variables]),
	term_to_atom(List2,List2a),
	concat_list([S1,List2a,',\n'],S2))->true;
	(List2=[[n,Name],Variables1,":-",Body]->
	(term_to_atom(Variables1,Variables2),
	concat_list([S1,'[[[n,',Name,']],\n',Variables2,
	',":-",\n['],String),
	pp2(Body,'',B1),
	concat_list([String,B1,',\n]],\n'],S2)))),!.
pp1(List1,S1,S2) :-
	List1=[List2|Lists3],
	(((List2=[[n,_Name]]->true;List2=[[n,_Name],
		_Variables]),
	term_to_atom(List2,List2a),
	concat_list([S1,List2a,',\n'],S3))->true;
	(List2=[[n,Name],Variables1,":-",Body]->
	(term_to_atom(Variables1,Variables2),
	concat_list([S1,'[[[n,',Name,']],',Variables2,
	',":-"\n['],String),
	pp2(Body,'',B1),
	concat_list([String,B1,',\n]],\n'],S3)))),
	pp1(Lists3,S3,S2),!.
pp2([],S,S):-!.
pp2(List1,S1,S2) :-
	List1=[List2],
	%write("\t")%,writeln(List2),
	term_to_atom(List2,List2a),
	concat_list([S1,'\t',List2a],S2),
	!.
pp2(List1,S1,S2) :-
	List1=[List2|Lists3],
	%write("\t"),write(List2),writeln(","),
	term_to_atom(List2,List2a),
	pp2(Lists3,'',S3),
	concat_list([S1,'\t',List2a,',',S3],S2),!.
	
/**
pp3([]) :- !.
pp3(List1) :-
	List1=[List2|List3],
	writeln1(List2),
	pp3(List3).
**/

/**
concat_list_term(List,String) :-
%trace,
	findall(A,(member(Item,List),
	%trace,
	(atom(Item) -> Item=A;
	term_to_atom(Item,A))
	%notrace
	),List1),
	concat_list(List1,String).
**/
	
contains_string(Atom) :-
	string_concat(A,B,Atom),
	string_length(A,1),
	A="\"",
	string_concat(_,C,B),
	string_length(C,1),
	C="\"",!.

% remove " if string, leave as atom if atom

string_atom2(String1,Atom1) :-
	contains_string(Atom1),
	delete1(Atom1,"\"",String1),
	%string_atom(String1,String2),
	%replace(String2,"'","#",String1),
	%string_atom(String1,String3),
	!.
string_atom2(String1,Atom1) :-
	atom(Atom1),%String1=Atom1,
	
	%replace(Atom1,"\"","&",String2),
	delete1(Atom1,"'",%"#",
	String3),
	
	string_atom(String3,String1),
	!.
	
replace(A1,Find,Replace,F) :-
string_concat("%",A1,A2),
string_concat(A2,"%",A),		split_string(A,Find,Find,B),findall([C,Replace],(member(C,B)),D),maplist(append,[D],[E]),concat_list(E,F1),string_concat(F2,G,F1),string_length(G,1),
	string_concat("%",F3,F2),	
	string_concat(F,"%",F3).

v_if_string_or_atom(String_or_atom,V) :-
	((string(String_or_atom)->true;
	atom(String_or_atom))->
	V=[v,String_or_atom];
	V=String_or_atom),!.
	
delete1(A	,Find,F) :-
%string_concat("%",A1,A2),
%string_concat(A2,"%",A),
		split_string(A,Find,"",B),%findall([C,Replace],(member(C,B)),D),
		maplist(append,[[B]],[E]),concat_list(E,F).%,string_concat(F,G,F1),string_length(G,1).
	%string_concat("%",F3,F2),	
	%string_concat(F,"%",F3).
