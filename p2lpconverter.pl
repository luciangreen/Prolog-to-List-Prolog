:-include('../listprologinterpreter/listprolog.pl').
:-include('pretty_print.pl').
:-include('pretty_print_lp2p.pl').
:-include('p2lpverify.pl').

%% p2lpconverter(S1),pp0(S1,S2),writeln(S2).

/**trace,
string_codes("a.\nb(C,D).\nef('A'):-(h(a)->true;true),!.",A),phrase(file(B),A),write(B).
**/

%% [[[n,a]],[[n,b],[[v,c],[v,d]]],[[n,ef],['A'],:-,[[[n,->],[[[n,h],[a]],[[n,true]],[[n,true]]]],[[n,cut]]]]]


use_module(library(pio)).
use_module(library(dcg/basics)).

%:- include('la_strings.pl').

:-dynamic keep_comments/1.

init_keep_comments :-
	(not(keep_comments(_)
	)->
	(retractall(keep_comments(_)),
 	assertz(keep_comments([])));
	true),!.
	
turn_keep_comments_on :-
	retractall(keep_comments(_)),
 	assertz(keep_comments([percentage_comments,slash_star_comments%,newlines
 	])),!.

turn_keep_comments_off :-
	retractall(keep_comments(_)),
 	assertz(keep_comments([])),!.

p2lpconverter(A,B) :-
	catch(call_with_time_limit(0.1,
	p2lpconverter1(A,B)),_,false),!.

p2lpconverter(A) :-
	catch(call_with_time_limit(0.1,
	p2lpconverter1(A)),_,false),!.

p2lpconverter1([string,String],List3) :-
	turn_keep_comments_on,
	%File1="test1.pl",
	string_codes(String,String1),
	(phrase(file(List3),String1)->true;%(writeln(Error),
	fail),!.

p2lpconverter1([file,File1],List3) :-
	turn_keep_comments_on,
	%File1="test1.pl",
	readfile(File1,"test1.pl file read error.",List3),!.


p2lpconverter1(List3) :-
	turn_keep_comments_on,
	File1="test1.pl",	
	readfile(File1,"test1.pl file read error.",List3),!.

readfile(List1,_Error,List3) :-
	%init_keep_comments,
	phrase_from_file_s(string(List6), List1),
	(phrase(file(List3),List6)->true;%(writeln(Error),
	fail).
	%writeln1(List3)	.

string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

%file(N) --> newlines1(N),!.

file(Ls2) --> newlines1(N0),file2([],Ls3),
 {foldr(append,[
 N0,
 Ls3
 %Ls,
 %N0,N1
 ],Ls2)}.
file(N0) --> newlines1(N0).
%file2([],[]) --> [].

file2(N0,Ls2) --> %newlines1(N1),
predicate(L),%
newlines1(N1),
	%{trace},
%{writeln1(L)},
%file2(N1,Ls),
%{writeln1(L)}, %%***
 {foldr(append,[
 N0,N1,L
 %Ls,
 ],Ls2)}
 %delete(Ls3,[],Ls2)},
 . 
file2(N0,Ls2) --> %newlines1(N1),
predicate(L),%
newlines1(N1),

%newlines1(N11),
	%{trace},
%{writeln1(L)},
file2(N1,Ls),
%{writeln1(L)}, %%***
 {foldr(append,[
 L,
 Ls,N0%,N1%,N11
 ],Ls2)}
 %delete(Ls3,[],Ls2)},
. 
file2(N0,Ls2) --> newlines1(Ls2),!.
%file2(N,N) --> [],!.

/*
file(Ls2) --> newlines1(N),file(Ls),
%{writeln1(L)}, %%***
 {%foldr(append,[[L],N|Ls],Ls3),
 delete([N|Ls],[],Ls2)},
%{foldr(append,[N],Ls2)},
%{writeln1(L)},
!.
*/
%%predicate([]) --> newlines1(_).
%predicate_test([]) --> "a",".".

predicate(A) -->
		":-",newlines1(_),name1(Word11),
		"(",newlines1(N1),varnames(Varnames),")",
		newlines1(N2),
		 ".",
		{foldr(append,[[[":-",[n,Word11],Varnames
		]],N1,N2
		],A)
		}.
predicate(A) -->
		":-",newlines1(_),name1(Word11),
		newlines1(N1),name1(Word13),
		"/",newlines1(N2),name1(Word12),
		newlines1(N3),
		 ".",
		{foldr(append,[[[":-",[n,Word11],[Word13,"/",Word12]
		]],N1,N2,N3
		],A)
		}.

predicate(A) -->
		name1(Word11), predicate(Word11,A).
predicate([]) --> [],!.

%predicate(A) -->
%		name1(Word11), predicate(Word11,A).
		

%predicate(A2) -->
%		name1(Word11),predicate(Word11,A).

%predicate(A2) -->
%		name1(Word11),predicate(Word11,A).

%predicate(A2) -->
%		name1(Word11),predicate(Word11,A).
predicate(Word11,A) -->
	 	newlines1(N1),predicate2(N1,Word11,A).

predicate(Word11,A) -->		"(",newlines1(N1),varnames(Varnames),newlines1(N2),
		")",predicate(N1,N2,Word11,Varnames,A).
predicate(%N1,
Word11,A) -->		
		".", %{A=[[[n,Word11]]]
		%}
		{foldr(append,[[[[n,Word11]]],N1
		],A)
		}.		

predicate2(N1,Word11,A) -->		
		"-->",newlines1(N2),
		lines(L), ".",
		{foldr(append,[[[[n,Word11],"->",%N,
		L]],N1,N2],A)
		}.

%predicate(Word11,A) -->		
%		newlines1(N1),predicate2(Word11,A).
predicate2(N1,Word11,A) -->		
		":-",newlines1(N2),%{trace},
		lines(L), ".",
		{foldr(append,[[[[n,Word11],":-",%N,
		L]],N1,N2],A)
		}.		
%predicate(Word11,A) -->		
%		newlines1(N1),predicate2(Word11,A).
predicate2(N1,Word11,A) -->		
		"->",newlines1(N2),
		lines(L), ".",
		{foldr(append,[[[[n,Word11],"->",%N,
		L]],N1,N2],A)
		}.

predicate(N1,N2,Word11,Varnames,A) -->		
		".", {foldr(append,[[[[n,Word11],Varnames]],N1,N2],A)
		}.
predicate(N1,N2,Word11,Varnames,A) -->
	newlines1(N3),predicate3(N1,N2,N3,Word11,Varnames,A).		
/*
predicate(A2) -->
		name1(Word11),
		"(",newlines1(N1),varnames(Varnames),")",*/
predicate3(N1,N2,N3,Word11,Varnames,A) -->
		":-",newlines1(N4),%{trace},
		lines(L), ".",
		{foldr(append,[[[[n,Word11],Varnames,":-",%N,
		L]],N1,N2,N3,N4
		],A)
		%delete(A,[],A2)
		}.
%predicate(Word11,Varnames,A) -->		

/*predicate(A2) -->
		name1(Word11),
		"(",newlines1(N1),varnames(Varnames),")",*/
predicate3(N1,N2,N3,Word11,Varnames,A) -->
		"->",newlines1(N4),
		lines(L), ".",
		{foldr(append,[[[[n,Word11],Varnames,"->",
		L]],N1,N2,N3,N4],A)
				%delete(A,[],A2)

		}.
%predicate(Word11,Varnames,A) -->		
/*predicate(A2) -->
		name1(Word11),
		"(",newlines1(N1),varnames(Varnames),")",*/
predicate3(N1,N2,N3,Word11,Varnames,A) -->
		"-->",newlines1(N4),
		lines(L), ".",
		{foldr(append,[[[[n,Word11],Varnames,"->",%N,
		L]],N1,N2,N3,N4],A)
				%delete(A,[],A2)

		}.
		/**name1([L3|Xs]) --> [X], {string_codes(L2,[X]),(char_type(X,alnum)->true;L2="_"),downcase_atom(L2,L3)}, name1(Xs), !.
name1([]) --> [].
**/


name1(X1) --> name1a(X1).

name1a(X1) --> name10(X11),
	{(string_atom(X12,X11),number_string(X1,X12)->true;
	((%contains_string(X11)->
	string_atom2(X1,X11)%;X11=X1)
	)))}.%%., X2->X1 {atom_string(X2,X1)}.

name1a(X1) --> name2(X1).


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

name11(X1) --> %{trace},
name101(X11),
	{(string_atom(X12,X11),number_string(X1,X12)->true;
	((%contains_string(X11)->
	string_atom2(X1,X11)%;X11=X1)
	)))}.%%., X2->X1 {atom_string(X2,X1)}.

/**
\"'a'\"
\"\\\"a\\\"\"
'"a"'
'\\'a\\''
**/

name101(XXs) --> "'",name1010(XXs1),"'", 
	{atom_concat_list(['\'',XXs1,'\''],XXs)}, !. 
	
name1010(XXs) --> [X],[Y], 
	{char_code(Ch1,X),char_code(Ch2,Y),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	Ch1='\\',Ch2='\'',
	atom_string(CA2,Ch1),atom_string(CA22,Ch2)},%%downcase_atom(CA,CA2)},
	name1010(Xs), 
	{atom_concat_list([CA2,CA22,Xs],XXs)}, !. 
name1010(XXs) --> [X], [Y],
	{char_code(Ch1,X),char_code(Ch2,Y),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	Ch1='\\',Ch2='\'',
	atom_string(CA2,Ch1),atom_string(CA22,Ch2)},%%downcase_atom(CA,CA2)},
	%name101(Xs), 
	{atom_concat_list([CA2,CA22,''],XXs)}, !. 

name1010(XXs) --> [X],
{char_code(Ch1,X),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	not(Ch1='\''),%not(Ch1='('),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	name1010(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 
name1010(XXs) --> [X], 
	{char_code(Ch1,X),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	not(Ch1='\''),%not(Ch1='('),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	%name101(Xs), 
	{atom_concat(CA2,'',XXs)}, !. 


name101(XXs) --> "\"",name1011(XXs1),"\"", 
	{atom_concat_list(['"',XXs1,'"'],XXs)}, !. 
	
	/*
name1011(XXs) --> [X], 
	{char_code(Ch1,X),%char_code(Ch2,Y),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	Ch1='"',%trace,%Ch2='"',
	atom_string(CA2,Ch1)%,atom_string(CA22,Ch2)
	},%%downcase_atom(CA,CA2)},
	%name1011(Xs), 
	{atom_concat_list([%'\'',CA2
	],XXs)}, !. 
*/
name1011(XXs) --> [X],[Y], 
	{char_code(Ch1,X),char_code(Ch2,Y),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	Ch1='\\',Ch2='"',
	atom_string(CA2,Ch1),atom_string(CA22,Ch2)},%%downcase_atom(CA,CA2)},
	name1011(Xs), 
	{atom_concat_list([CA2,CA22,Xs],XXs)}, !. 
name1011(XXs) --> [X], [Y],
	{char_code(Ch1,X),char_code(Ch2,Y),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	Ch1='\\',Ch2='"',
	atom_string(CA2,Ch1),atom_string(CA22,Ch2)},%%downcase_atom(CA,CA2)},
	%name101(Xs), 
	{atom_concat_list([CA2,CA22,''],XXs)}, !. 

name1011(XXs) --> [X], 
	{char_code(Ch1,X),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	not(Ch1='"'),%not(Ch1='('),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	name1011(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 
name1011(XXs) --> [X], 
	{char_code(Ch1,X),%char_type(X,ascii),%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	not(Ch1='"'),%not(Ch1='('),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	%name101(Xs), 
	{atom_concat(CA2,'',XXs)}, !. 



name101(XXs) --> name1012(XXs1),
	{atom_concat_list([XXs1],XXs)}, !. 

name1012(XXs) --> 
	[X],
	lookahead2([',',')',']','|'
		]),
	{char_code(Ch1,X),%char_type(X,ascii),
	%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	not(Ch1='['),not(Ch1=']'),not(Ch1='('),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	%name101(Xs), 
	{atom_concat(CA2,'',XXs)}, !. 


name1012(XXs) --> %{trace},
	[X], %lookahead2([',',')'%,']'
	%]),
	%{trace},
	%lookahead3(A),
	{%char_code(ChA,A),not(ChA=','),not(ChA=')'),
	char_code(Ch1,X),%char_type(X,ascii),
	%->true;(Ch1='\''->true;(Ch1='"'->true;(Ch1='_'->true;
	%Ch1='!'->true;Ch1='.')))),
	%not(Ch1=','),
	not(Ch1='['),not(Ch1=']'),not(Ch1='('),
	atom_string(CA2,Ch1)},%%downcase_atom(CA,CA2)},
	name1012(Xs), 
	{atom_concat(CA2,Xs,XXs)}, !. 


name2(X1) --> name20(X1).%%, {atom_string(X2,X1)}.


name20(XXs) --> [X], %{trace},
%lookahead(Y),
	{char_code(Ch1,X),%%char_type(X,alnum)->true;
	%trace,
	%writeln(Y),
	%(Ch1='-'->trace;true),
	%((Ch1='-',[Y]=`>`)->fail;(
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
%spaces1([X|Xs]) --> [X], {char_type(X,space)}, spaces1(Xs), !.
%spaces1([]) --> [].
%spaces1(_)-->newlines1(_).
/**
varnames([L1|Ls]) --> varname1(L1),",", %%{writeln(L)}, %%***
varnames(Ls), !. 
varnames([L1]) --> varname1(L1),
!.
**/

varnames01(L1) --> %{trace},
"[",newlines1(_N1),
varnames0(L2),newlines1(_),
"]",newlines1(_N2), 
{%foldr(append,[[L2],N1,N2],L1)
L1=L2},
!. 

/*
varnames01(L1) --> {trace},
"[",newlines1(N1),varnames0(L2),%,newlines1(_),
"]",newlines1(N2), 
{foldr(append,[[L2],N1,N2],L1)},
!.
*/

varnames01(L1) --> varname1(L2),
	{L1=L2},!.

/*
varnames01(L1) --> %{trace},
"[",%newlines1(N1),
varname1(L2),%,newlines1(_),
"]",%newlines1(N2), 
{foldr(append,[[L2]],L1)},
!.
*/

varnames(L3) --> %{trace},
"[",%newlines1(N1),
varnames0(L1),%newlines1(_),
"]",%newlines1(N2),
",",
%newlines1(N3),
varnames(L2),
	{foldr(append,[[L1],L2],L3)
	%foldr(append,[[L4]],L3)
	},
	%{append([L1],L2,L3)},
	!. 
% 	{maplist(append,[[[[L1],L2]]],[[L3]])},!. 


varnames(L3) --> %{trace},
"[",newlines1(_),varnames0(L1),newlines1(_),"]",newlines1(_),"|",varnames(L2),newlines1(_),
	{maplist(append,[[[[L1],"|",L2]]],[L3])},!. 



varnames(L1) --> %{trace},
"[",newlines1(_),varnames0(L2),newlines1(_),"]",newlines1(_),
{L1 = [L2]},
!. 


/*
varnames(L1) --> %{trace},
%"[","]",",","[","]",
varnames0(L0),
newlines1(_),spaces1(_),varnames0(L2),
	{append(L0,L2,L1)},!. 
	*/
	
varnames(L3) --> %{trace},
%"[",
%newlines1(_),
varnames0(L3),newlines1(_),%"]",
%",",
%newlines1(_),spaces1(_),varnames(L2),
	%{append(L1,L2,L3)},
	!. 

varnames(L1) --> %{trace},
"[",newlines1(_),"]",newlines1(_),",",newlines1(_),varnames(L2),
	{append([[]],L2,L1)},!. 

varnames(L1) --> %{trace},
"[",newlines1(_),"]",newlines1(_),"|",newlines1(_),varnames(L2),
	{maplist(append,[[[[[]],"|",L2]]],[L1])},!. 



varnames(L1) --> %{trace},
"[",newlines1(_),"]", newlines1(_),
{L1 = []},
!. 

varnames(L1) --> %{trace},
varnames0(L1), 
!. 

varnames0(L1) --> varname1(L2),%{trace},
lookahead1,%{notrace},
	{L1=[L2]},!.

varnames0(Ls2) --> %{trace},
varname1(L1),newlines1(_),",", newlines1(_),%%{writeln(L)}, %%***
	varnames0(Ls), newlines1(_),
	{append([L1],Ls,Ls2)},!. 
%	{maplist(append,[[[L1,Ls]]],[[Ls2]])},!. 

varnames0(Ls2) --> varname1(L1),newlines1(_),"|", newlines1(_),%%{writeln(L)}, %%***
	varnames0([Ls]), newlines1(_),
	{append_list([L1,"|",Ls],Ls2)},!. 
%	{maplist(append,[[[L1,"|",Ls]]],[Ls2])},!. 

varnames3(L1) --> varname1(L2),%{trace},
%lookahead1,%{notrace},
	{L1=L2},!.

varnames3(L1) --> varnames(L1),%{trace},
%lookahead1,%{notrace},
	%{L1=[L2]},
	!.

lookahead1(A,A) :- append(`]`,_,A).
lookahead1(A,A) :- append(`)`,_,A).
%lookahead1(A,A) :- append(`.`,_,A).
%lookahead1(A,A) :- append(`,`,_,A).
%lookahead1(A,A) :- append(`|`,_,A).

%lookahead3(A,A) :- lookahead1(A,A)
%lookahead3(A,A) :- append(`,`,_,A).

%lookahead(_,[],[]) :-!.
lookahead(B2,A,A) :-
%trace,
	%member(B,B1),
	%string_codes(B,B2),
	append([B2],_D,A),!.

lookahead2(B1,A,A):-
%trace,
	member(B,B1),
	string_codes(B,B2),
	append(B2,_D,A).

varname1([]) --> "[",newlines1(_),"]",newlines1(_). %%{writeln(L)}, %%***
varname1(L4) --> %{trace},
name11(L1), newlines1(_),%%{writeln(L)}, %%***
{%trace,%%atom_string(L1,L10),string_codes(L2,L10),
(((string(L1)->true;(atom_concat(A,_,L1),atom_length(A,1),(not(is_upper(A)),not(A='_'))))->L4=L1;(downcase_atom(%%L2
L1,L3),L4=[v,L3])))%%L3A

%%,term_to_atom(L3A,L4)%%,atom_string(L4A,L4)
}.
varname1(L4) --> "(",newlines1(_),line(L4),newlines1(_),")",newlines1(_).
varname1(L1) --> 
"[",newlines1(_),varnames0(L2),newlines1(_),"]",newlines1(_), 
{L1 = L2},!.

varname1(A) --> varname_term(A).
%varnames(A) --> varnames_term(B),","varnames(C),[].

varname_term(A) --> {%trace,
true},name1(Word11),{%(Word11=phrase_from_file->trace;true)%
%writeln1(Word11)
true
},
		"(",newlines1(_),varnames(Varnames),")",
		newlines1(_), {A=[[n,Word11],Varnames]%,trace
		},!.

%comment([]) --> [].
comment(X1) --> %spaces1(_),
[X], {char_code('%',X)},comment1(Xs), {append([X],Xs,X2),string_codes(X3,X2),X1=[[[n,comment],[X3]]]},!.

%comment1([]) --> [], !.
comment1([X|Xs]) --> %{trace},
[X], %lookahead(_A),
{not(char_type(X,newline))%,not(A=[])
}, comment1(Xs), !.
%%newlines1([X]) --> [X], {char_type(X,newline)},!.
%comment1([X]) --> [X], %lookahead(A), 
%!.
%{(char_type(X,newline)->true;A=[])}, !.
comment1([]) --> [X], lookahead(A), {(char_type(X,newline)->true;A=[])}, !.
comment1([]) --> [], !.


comment2(X1) --> %spaces1(_),
[XA],[XB], {char_code('/',XA),char_code('*',XB)},comment3(Xs), {flatten([XA,XB|Xs],X4),%foldr(append,X4,X2),
string_codes(X3,X4),X1=[[[n,comment],[X3]]]},!.

%comment1([]) --> [], !.
comment3([XA|Xs]) --> [XA],%[XB],
 lookahead(XB),{not((char_code('*',XA),char_code('/',XB)))}, comment3(Xs), !.
%%newlines1([X]) --> [X], {char_type(X,newline)},!.
comment3([XA,XB]) --> [XA],[XB], {char_code('*',XA),char_code('/',XB)}, !.


%newlines1(X3) --> newlines0(X),newlines1(X2),{append(X,X2,X3)},!.
/*
newlines1(X) --> newlines0(X).
newlines0(X) --> newlines11(X2), {%(not(X2=[])->
X=[[[n,newlines],[X2]]]%;X=[])
},!.
newlines0(X) --> newlines12(X2), {X=[[[n,percentage_comments],[X2]]]},!.
newlines0(X) --> newlines13(X2), {X=[[[n,slash_star_comments],[X2]]]},!.
newlines00(X) --> newlines11(X).
newlines00(X) --> newlines12(X).
newlines00(X) --> newlines13(X).
*/
newlines1(Xs) --> [X], {char_type(X,space)}, newlines1(Xs),
%{%keep_comments(Y),
%(%true%member(newlines,Y)
%->append([X],Xs,Xs2);Xs2=[])
%},
!.
newlines1(Xs2
) --> %{trace},
comment(X), newlines1(Xs), %{append([X],Xs,Xs2)},
{keep_comments(Y),(member(percentage_comments,Y)->append(X,Xs,Xs2);Xs2=[])},
!.
newlines1(Xs2
) --> comment2(X), newlines1(Xs), %{append([X],Xs,Xs2)},
{keep_comments(Y),(member(slash_star_comments,Y)->append(X,Xs,Xs2);Xs2=[])},
!.
%%newlines1([X]) --> [X], {char_type(X,newline)},!.
%newlines1([]) --> [],%lookahead([]),
%!.
newlines1([]) --> [],%lookahead([]),
!.

/*
newlines11([]) --> [],%lookahead([]),
!.
newlines12([]) --> [],%lookahead([]),
!.
newlines13([]) --> [],%lookahead([]),
!.
*/

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
comma_or_semicolon --> ",".
comma_or_semicolon --> ";"%,{trace}
.

lines(Ls2) --> %{trace},%newlines1(_),
%newlines1(_),
line(L),newlines1(N1),comma_or_semicolon,%",",
newlines1(N2),
%{writeln(L)}, %%***
lines(Ls), %trace,
newlines1(N3),
%{delete([L,N|Ls],[],Ls2)}, !. 
%lines(Ls2) --> line(L),",",newlines1(N),
%%{writeln(L)}, %%***
%lines(Ls), 
{foldr(append,[[L],N1,N2,
Ls,N3
],Ls2%[],Ls2
)}, !. 
lines([L]) --> line(L), 
%%{writeln(L)},
!.
varname_or_names(Varnames1) --> varnames([Varnames1]).
varname_or_names(Varname) --> varname1(Varname).

% 	maplist(append,[[[40],B1,[41]]],[B12]), % "()"

%varname1
line(A) -->%{trace},
		varname_or_names(Varnames1),newlines1(_),"=",newlines1(_),
		varname_or_names(Varnames2),%newlines1(_),
		{A=[[n,equals4],[Varnames1,Varnames2]]
		}.
/*
line(A) -->%{trace},
		"true",%newlines1(_),
		{trace},
		{A=[[n,true]]
		}.
*/

line(A) --> %%spaces1(_), 
		name1(Word11), %% name(A,B,C)
		{%trace,
		Word11=not},
		"(",newlines1(_),
		lines(Lines),")",
		{A=[[n,Word11],Lines]},!.

line(A) --> %%spaces1(_), 
		name1(Word11),newlines1(_), %% name(A,B,C).
		%{trace,(Word11="->"->trace;true)},
		{%trace,
		not(Word11=findall)},
		"(",newlines1(_),varnames(Varnames),")",
		{A=[[n,Word11],Varnames]},!.
line(A) --> %%spaces1(_), 
		name1(Variable1),newlines1(_),{%trace,
		not(Variable1=findall)},
		newlines1(_), %% A = B*Y
		(name1(_Is)|name2(_Equals)), newlines1(_), 
		name1(Variable2), 	newlines1(_),
		name2(Operator),newlines1(_), 
		{%trace,
		not(Operator=(->))},
		name1(Variable3), 	%newlines1(_),
		{ %% A=B*Y 
		v_if_string_or_atom(Variable2,Variable2a),
		v_if_string_or_atom(Variable3,Variable3a),
		v_if_string_or_atom(Variable1,Variable1a),
		A=[[n,Operator],[Variable2a,Variable3a,Variable1a]]},!.
line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		newlines1(_), %% A is B
		name2(Word21), newlines1(_), name1(Word11),
		{v_if_string_or_atom(Word10,Word10a),
		v_if_string_or_atom(Word11,Word11a),
		A=[[n,Word21],[Word10a,Word11a]]},!.
line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		newlines1(_), %% A is B
		name1(Word21), newlines1(_), name1(Word11),
		{v_if_string_or_atom(Word10,Word10a),
		v_if_string_or_atom(Word11,Word11a),
		A=[[n,Word21],[Word10a,Word11a]]},!.
/*line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		"is",
		%name2(Word21),
		 %spaces1(_), 
		name1(Word11),
		{v_if_string_or_atom(Word10,Word10a),
		v_if_string_or_atom(Word11,Word11a),
		A=[[n,=],[Word10a,Word11a]]},!.
		*/
line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		"=",
		%name2(Word21),
		 %spaces1(_), 
		"[",
		"]",
		{v_if_string_or_atom(Word10,Word10a),
		%v_if_string_or_atom(Word11,Word11a),
		%v_if_string_or_atom(Word12,Word12a),
		A=[[n,equals4],[Word10a,[]]]},!.
line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		"=",
		%name2(Word21),
		 %spaces1(_), 
		"\"\"",
		{v_if_string_or_atom(Word10,Word10a),
		%v_if_string_or_atom(Word11,Word11a),
		%v_if_string_or_atom(Word12,Word12a),
		A=[[n,equals4],[Word10a,""]]},!.
line(A) --> %%spaces1(_), 
   %{trace},
		name1(Word10),newlines1(_),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		"=",newlines1(_),
		%name2(Word21),
		 %spaces1(_), 
		"[",newlines1(_),
		varnames(Word11),newlines1(_),"]",%newlines1(_),
		{v_if_string_or_atom(Word10,Word10a),
		%v_if_string_or_atom(Word11,Word11a),
		%v_if_string_or_atom(Word12,Word12a),
		A=[[n,equals4],[Word10a,Word11]]},!.
line(A) --> %%spaces1(_), 
   %{trace},
		%name1(Word10),{%trace,
		%not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		%"=",
		%name2(Word21),
		 %spaces1(_), 
		"[",newlines1(_),
		varnames(Word11),newlines1(_),"]",%newlines1(_),
		{%v_if_string_or_atom(Word10,Word10a),
		%v_if_string_or_atom(Word11,Word11a),
		%v_if_string_or_atom(Word12,Word12a),
		A=Word11},!.
line(A) --> %%spaces1(_), 
   %{trace},
		%name1(Word10),{%trace,
		%not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		%"=",
		%name2(Word21),
		 %spaces1(_), 
		"[",newlines1(_),
		%varnames(Word11),
		"]",%newlines1(_),
		{%v_if_string_or_atom(Word10,Word10a),
		%v_if_string_or_atom(Word11,Word11a),
		%v_if_string_or_atom(Word12,Word12a),
		A=[]},!.
		
line(A) --> %%spaces1(_), 
		name1(Word10),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		"=",
		%name2(Word21),
		 %spaces1(_), 
		"[",name1(Word11),
		"|",name1(Word12),
		"]",
		{v_if_string_or_atom(Word10,Word10a),
		v_if_string_or_atom(Word11,Word11a),
		v_if_string_or_atom(Word12,Word12a),
		A=[[n,equals4],[Word10a,[Word11a,"|",Word12a]]]},!.
line(A) --> %%spaces1(_), 
		name1(Word10),newlines1(_),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A = [B,C]
		"=",newlines1(_),
		%name2(Word21),
		 %spaces1(_), 
		varnames3(Word11),%newlines1(_),
		{v_if_string_or_atom(Word10,Word10a),
		%v_if_string_or_atom(Word11,Word11a),
		A=[[n,=],[Word10a,Word11]]},!.
line(A) --> %%spaces1(_),
		%{trace}, 
		name1(Word10),{%trace,
		not(Word10=findall)},
		%spaces1(_), 
		%% A=B
		"=",
		%name2(Word21),
		 %spaces1(_), 
		name1(Word11),
		{v_if_string_or_atom(Word10,Word10a),
		v_if_string_or_atom(Word11,Word11a),
		A=[[n,=],[Word10a,Word11a]]},!.

line(A) --> %%spaces1(_), 
		name1(Word11), newlines1(_),%% name(A,B,C)
		{%trace,
		Word11=findall},
		"(",newlines1(_),
		varnames01(Varnames),newlines1(_),",",newlines1(_),
		"(",newlines1(_),lines(A1),newlines1(_),")",newlines1(_),",",
		newlines1(_),
		varnames01(Varnames2),newlines1(_),")",%newlines1(_),
		{A=[[n,Word11],[Varnames,A1,Varnames2]]},!.

line(A) --> %%spaces1(_), 
		name1(Word11), newlines1(_),%% name(A,B,C)
		{%trace,
		Word11=findall},
		"(",newlines1(_),varnames01(Varnames),newlines1(_),",",newlines1(_),
		line(A1),newlines1(_),",",newlines1(_),
		varnames01(Varnames2),newlines1(_),")",%newlines1(_),
		{A=[[n,Word11],[Varnames,A1,Varnames2]]},!.
		
line(Word1) -->
		"{",line(Word2),"}",{Word1=[[n,code],Word2]},!.
		
line(Word1) -->
		"(",newlines1(_),line(Word2),")",{Word1=[Word2]},!.
line(Word1) -->%{trace},
		"(",newlines1(_),line(Word2),newlines1(_),"->",newlines1(_),
		line(Word3),newlines1(_),";",newlines1(_N2),
		line(Word4),newlines1(_N3),")",
		{%(Word4=[[[[n,_]|_]|_]|_]->Word4=[Word41];Word4=Word41),
		%if_one_item_then_remove_brackets()
		Word1=[[n,"->"],[Word2,Word3,Word4]]},!.
line(Word1) -->
		"(",newlines1(_),
		line(Word2),newlines1(_N21),"->",newlines1(_N2),line(Word3),newlines1(_N3),")",
		{Word1=[[n,"->"],[Word2,Word3]]},!.
line(Word1) -->
		"(",newlines1(_),
		line(Word2),";",newlines1(_N2),line(Word3),")",
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
		"(",newlines1(_),
		lines(Word2),")",
		{Word1=Word2},!.
line(Word1) -->
		"{",newlines1(_),lines(Word2),"}",
		{Word1=[[n,code],Word2]},!.

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
	
contains_string(Atom,String) :-
	string_concat(A,B,Atom),
	string_length(A,1),
	A="\"",
	string_concat(String,C,B),
	string_length(C,1),
	C="\"",!.

% remove " if string, leave as atom if atom

string_atom4(String,Atom) :- %trace,
((atom(String)->(
%	string_atom(Atom,String),!.

term_to_atom(A,Atom),
(var(A)->String=Atom;
atom_string(A,String))
);String=Atom)->((writeln1(string_atom2(String,Atom))));(writeln1(string_atom2(String,Atom)),false)).

% \" \\\"\" -> " \""
% or '" \\""' -> '" \""'

string_atom2(String1,Atom1) :-
%writeln1(string_atom2(String1,Atom1)),
	contains_string(Atom1,String2),%trace,
	%foldr(string_concat,String2,String1),
%trace,
	%String1=Atom1,
	%string_atom(String1,String2),
	%string_strings(Atom1,A),
	%append([_],A1,A),
	%append(A2,[_],A1),
	%foldr(string_concat,A2,String1),
	
	%delete1_p2lp(Atom1,"\"",String1),
 atomic_list_concat(A,"\\",String2),
 atomic_list_concat(A,"",Atom2),
 atom_string(Atom2,String1),
	
	%string_atom(String1,String2),
	%replace_p2lp(String2,"'","#",String1),
	%string_atom(String1,String2),
	!.
string_atom2(String1,Atom1) :-
	atom(Atom1),%String1=Atom1,
	%trace,
	%replace_p2lp(Atom1,"\"","&",String2),
	delete1_p2lp(Atom1,"'",%"#",
	String3),

	%string_strings(Atom1,A),
	%append([_],A1,A),
	%append(A2,[_],A1),
	%foldr(string_concat,A2,String3),
	
	string_atom(String3,String1),
	!.
	
replace_p2lp(A1,Find,Replace,F) :-
string_concat("%",A1,A2),
string_concat(A2,"%",A),		split_string(A,Find,Find,B),findall([C,Replace],(member(C,B)),D),maplist(append,[D],[E]),concat_list(E,F1),string_concat(F2,G,F1),string_length(G,1),
	string_concat("%",F3,F2),	
	string_concat(F,"%",F3).

v_if_string_or_atom(String_or_atom,V) :-
	(((string(String_or_atom)->true;
	atom(String_or_atom)),
	%trace,
	string_concat(A,_,String_or_atom),
	string_length(A,1)
	%,is_upper(A)
	)->
	V=[v,String_or_atom];
	V=String_or_atom),!.
	
delete1_p2lp(A	,Find,F) :-
%writeln1(delete1_p2lp(A	,Find,F)),
%string_concat("%",A1,A2),
%string_concat(A2,"%",A),
%trace,
string_strings(A,B),
(append([Find],C,B)->true;C=B),
(append(D,[Find],C)->true;D=C),
foldr(string_concat,D,F).		%split_string(A,Find,"",B),%findall([C,Replace],(member(C,B)),D),
		%maplist(append,[[B]],[E]),concat_list(E,F).%,string_concat(F,G,F1),string_length(G,1).
	%string_concat("%",F3,F2),	
	%string_concat(F,"%",F3).
