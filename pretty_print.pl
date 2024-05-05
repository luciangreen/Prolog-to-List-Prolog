% ["Medicine","MEDICINE by Lucian Green Head of State Head Ache Prevention 4 of 4.txt",0,algorithms,"32. I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend.  I did this by observing the hansard.  First, I found the hansard.  Second, I observed him listen to the politician.  Third, I observed him take notes.  In this way, I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend by observing the hansard."]

% * I did this by observing the hansard. 

% For nested if-then, findall

:-dynamic pp_separate_comma/1.

pretty_print(List,String) :-
	(pp0(List,String)->true;
	pp_1(List,String)).

symbol_1(":-","\":-\"").
symbol_1("->","\"->\"").

pp0_2(A,B) :- 
 retractall(pp_separate_comma(_)),
 assertz(pp_separate_comma("\n")),
 pp0(A,B),!.

pp0_3(A,B) :- 
 retractall(pp_separate_comma(_)),
 assertz(pp_separate_comma("")),
 pp0(A,B),!.

pp0(A,B) :-
%/*
 ((pp_separate_comma(_PSC)
 %not(var(PSC))
 )->
 true;(retractall(pp_separate_comma(_)),
 assertz(pp_separate_comma("")))),
 %*/
 pp01(A,B).
 
pp01([],'[]') :- !.
pp01(List,String2) :-
%trace,

%trace,
	pp1(List,'',String1),
	concat_list(['[\n',String1],String5),
	%replace(String3,"&","\"",String4),
	%replace(String3,"#","'",String5),
	string_concat(String6,B,String5),string_length(B,2),
	string_concat(String6,'\n]',String2),
	!.
pp1([],S,S):-!.
pp1([List1],S1,S2) :-
	%List1=[List2],
	pp3(List1,S1,S2).
pp1(List1,S1,S2) :-
	List1=[List2|Lists3],
	pp3(List2,S1,S3),
		pp1(Lists3,S3,S2),!.
pp2([],S,S,_N):-!.
pp2(List1,S1,S2,N) :-
	pp_separate_comma(PSC),
	List1=[List2],
	
	(List2=[[n,findall],[V1,Body,V2]]-> % if then else
	(N2 is N+1,
	pp2([Body],'',S4,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	length(Counter2,N2),
	findall('\t',member(_,Counter2),Ts21),
	concat_list(Ts21,Ts2),
	%pp2(Lists3,'',S3,N),
	term_to_atom(V1,V11),
	term_to_atom(V2,V21),

	concat_list([S1,'\n',Ts,'[[n,findall]',',','\n',Ts,'[',
	'\n',Ts2,V11,PSC,',','\n',
	 S4,PSC,',',%'\n',Ts,
	 '\n',
	'\n',Ts2,V21,
	 '\n',%Ts,'],[',
	 %S5,
	 Ts,']]'],S2));

	(List2=[[n,"->"],[If,Then]]-> % if then else
	(N2 is N+1,
	pp2([If],'',S4,N2),
	pp2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	%pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,"->"]',',','\n',Ts,'[',
	 S4,PSC,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,'\n',Ts,']]'],S2));

	%* remove \n Ts
	%* put in n,"->"
	(List2=[[n,"->"],[If,Then,Else]]-> % if then else
	(N2 is N+1,
	pp2([If],'',S4,N2),
	pp2([Then],'',S5,N2),
	pp2([Else],'',S51,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	%pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,"->"]',',','\n',Ts,'[',
	 S4,PSC,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,PSC,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S51,'\n',Ts,']]'],S2));
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',','\n',Ts,S51],S2));
	
	%write("\t")%,writeln(List2),
	(%pp2(List2,'',List2a,N),
	List2=[[n,_]|_],
	term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,'\n',Ts,List2a],S2));
	
	(pp2(List2,'',List2a,N),
	%term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,%'\n',Ts,
	'\n',Ts,'[',
	List2a,
	'\n',Ts,']'
	],S2))))),
	!.
pp2(List1,S1,S2,N) :-
	pp_separate_comma(PSC),
	List1=[List2|Lists3],
	%write("\t"),write(List2),writeln(","),

	(List2=[[n,findall],[V1,Body,V2]]-> % if then else
	(N2 is N+1,
	pp2([Body],'',S4,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	length(Counter2,N2),
	findall('\t',member(_,Counter2),Ts21),
	concat_list(Ts21,Ts2),
	%pp2(Lists3,'',S3,N),
	term_to_atom(V1,V11),
	term_to_atom(V2,V21),
	pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,findall]',',','\n',Ts,'[',
	'\n',Ts2,V11,PSC,',','\n',
	 S4,PSC,',',%'\n',Ts,
	 '\n',
	'\n',Ts2,V21,
	 '\n',%Ts,'],[',
	 %S5,
	 Ts,']]',PSC,',',S3],S2));

	(List2=[[n,"->"],[If,Then]]-> % if then else
	(N2 is N+1,
	pp2([If],'',S4,N2),
	pp2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,"->"]',',','\n',Ts,'[',
	 S4,PSC,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,'\n',Ts,']]',PSC,',',S3],S2));
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',',S3],S2));
	
	(List2=[[n,"->"],[If,Then,Else]]-> % if then else
	(N2 is N+1,
	pp2([If],'',S4,N2),
	pp2([Then],'',S5,N2),
	pp2([Else],'',S51,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,"->"]',',','\n',Ts,'[',
	 S4,PSC,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,PSC,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S51,'\n',Ts,']]',PSC,',',S3],S2));
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',','\n',Ts,S51,',',S3],S2));
	(%pp2(List2,'',List2a,N),
	%trace,
	List2=[[N_or_v,_]|_],(N_or_v=n->true;N_or_v=v),
	pp2(Lists3,'',S3,N),
	term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	(S3=''->Comma='';Comma=','),
	concat_list([S1,'\n',Ts,List2a,Comma,S3],S2));
	
	(pp2(List2,'',List2a,N),
	pp2(Lists3,'',S3,N),
	%term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,%'\n',Ts,
	'\n',Ts,'[',
	List2a,
	'\n',Ts,']',PSC,
	',',S3
	],S2))))),
	%concat_list([S1,%'\n',Ts,
	%List2a,',',S3],S2)))),

/*	

	(term_to_atom(List2,List2a),
	pp2(Lists3,'',S3,N),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,'\n',Ts,List2a,',',S3],S2)))),
	*/
	!.

pp_1(List,String) :-
	term_to_atom(List,Atom),
	atom_string(Atom,String).

pp3(List1,S1,S3) :-
	pp_separate_comma(PSC),
	symbol_1(Symbol,Symbol1),
	List1=List2,
	(((List2=[[_N10,_Name]]->true;List2=[[_N10,_Name],
		_Variables]),
	term_to_atom(List2,List2a),
	concat_list([S1,List2a,PSC,',\n'],S3))->true;
	
	((List2=[":-",[_,_Word11],_Varnames%N,
		]%,N3
		;List2=[":-",[_,_Word11],[_Word13,"/",_Word12]%,Varnames%N,
		]
		),
	term_to_atom(List2,List2a),
	concat_list([S1,List2a,PSC,',\n'],S3))->true;
	
	((List2=[[N1,Name],Variables1,Symbol,Body]->
	(term_to_atom(Variables1,Variables2),
	concat_list([S1,'[[',N1,',',Name,'],',Variables2,
	PSC,',',Symbol1,',\n[',PSC],String),
	%trace,
	pp2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([String,B1,'\n]]',PSC,',\n'],S3)))->true;
	List2=[[N1,Name],Symbol,Body]->
	(%term_to_atom(Variables1,Variables2),
	concat_list([S1,'[[',N1,',',Name,']',PSC,',',Symbol1,',\n[',PSC],String),
	%trace,
	pp2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([String,B1,'\n]]',PSC,',\n'],S3)))),!.
