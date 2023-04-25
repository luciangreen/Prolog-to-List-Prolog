% ["Medicine","MEDICINE by Lucian Green Head of State Head Ache Prevention 4 of 4.txt",0,algorithms,"32. I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend.  I did this by observing the hansard.  First, I found the hansard.  Second, I observed him listen to the politician.  Third, I observed him take notes.  In this way, I prepared to observe the people like my lecturer friend and meditation student was a doctor and friend by observing the hansard."]

% * I did this by observing the hansard. 

% For nested if-then, findall

:-include('../List-Prolog-to-Prolog-Converter/lp2pconverter.pl').

pretty_print_lp2p(List,String) :-
	(pp_lp2p0(List,String)->true;
	pp_lp2p_1(List,String)).

symbol_1_lp2p(":-",":-").
symbol_1_lp2p("->","->").

pp_lp2p0([],'') :- !.
pp_lp2p0(List,String2) :-
%trace,
	pp_lp2p1(List,'',String1),
	concat_list(['',String1],String6),%5),
	%replace(String3,"&","\"",String4),
	%replace(String3,"#","'",String5),
	%string_concat(String6,B,String5),string_length(B,2),
	string_concat(String6,'\n',String2),
	!.
pp_lp2p1([],S,S):-!.
pp_lp2p1([List1],S1,S2) :-
	%List1=[List2],
	pp_lp2p3(List1,S1,S2).
pp_lp2p1(List1,S1,S2) :-
	List1=[List2|Lists3],
	pp_lp2p3(List2,S1,S3),
		pp_lp2p1(Lists3,S3,S2),!.
pp_lp2p2([],S,S,_N):-!.
pp_lp2p2(List1,S1,S2,N) :-
	List1=[List2],
	
	(List2=[[n,findall],[V1,Body,V2]]-> % if then else
	(N2 is N+1,
	pp_lp2p2([Body],'',S4,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	length(Counter2,N2),
	findall('\t',member(_,Counter2),Ts21),
	concat_list(Ts21,Ts2),
	%pp_lp2p2(Lists3,'',S3,N),
	%term_to_atom(V1,V11),
	interpretstatementlp2p5(V1,'',V11),
	%term_to_atom(V2,V21),
	interpretstatementlp2p5(V2,'',V21),
	concat_list([S1,'\n',Ts,'findall(',%'\n',Ts,
	%'\n',Ts2,
	V11,',','\n',
	 S4,',',%'\n',Ts,
	 '\n',
	'\n',Ts2,V21,
	 %'\n',%Ts,'],[',
	 %S5,
	 ')'%Ts,']]'
	 ],S2));

	(List2=[[n,"->"],[If,Then]]-> % if then else
	(N2 is N+1,
	pp_lp2p2([If],'',S4,N2),
	pp_lp2p2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	%pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,%'[[n,"->"]',',',
	%'\n',Ts,
	 '(',
	 S4,'->',%'\n',Ts,
	 '\n',Ts,'',
	 S5,'\n',Ts,')'],S2));
	 
	(List2=[[n,code],Code1]-> % if then else
	(N2 is N+1,
	(%trace,
	(Code1=[A|_],not(predicate_or_rule_name(A)))->
	Code1=Code;[Code1]=Code),
	pp_lp2p2(Code,'',S4,N2),
	%pp_lp2p2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	%pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,%'[[n,"->"]',',',
	%'\n',Ts,
	 '{',
	 S4,'',%'\n',Ts,
	 %'\n',Ts,'',
	 %S5,
	 '\n',Ts,'}'],S2));

	%* remove \n Ts
	%* put in n,"->"
	(List2=[[n,"->"],[If,Then,Else]]-> % if then else
	(N2 is N+1,
	pp_lp2p2([If],'',S4,N2),
	pp_lp2p2([Then],'',S5,N2),
	pp_lp2p2([Else],'',S51,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	%pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,%'[[n,"->"]',',','\n',Ts,
	'(',
	 S4,'->',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,';',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S51,'\n',Ts,')'],S2));
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',','\n',Ts,S51],S2));
	
	%write("\t")%,writeln(List2),
	(%pp_lp2p2(List2,'',List2a,N),
	List2=[[n,cut]|_]->
	(%pp4_lp2p3_1_4(List2,List2a),
	List2a="!",
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,'\n',Ts,List2a],S2));

	(%pp_lp2p2(List2,'',List2a,N),
	List2=[[n,_]|_]->
	(pp4_lp2p3_1_4(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,'\n',Ts,List2a],S2));
	
	(N2 is N+1,
	pp_lp2p2(List2,'',List2a,N2),
	%term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,%'\n',Ts,
	'\n',Ts,'(',
	List2a,
	'\n',Ts,')'
	],S2)))))))),
	!.
pp_lp2p2(List1,S1,S2,N) :-
	List1=[List2|Lists3],
	%write("\t"),write(List2),writeln(","),

	(List2=[[n,findall],[V1,Body,V2]]-> % if then else
	(N2 is N+1,
	pp_lp2p2([Body],'',S4,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	length(Counter2,N2),
	findall('\t',member(_,Counter2),Ts21),
	concat_list(Ts21,Ts2),
	%pp_lp2p2(Lists3,'',S3,N),
	interpretstatementlp2p5(V1,'',V11),
	interpretstatementlp2p5(V2,'',V21),
	pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'findall(',%'\n',Ts,'',
	%'\n',Ts2,
	V11,',','\n',
	 S4,',',%'\n',Ts,
	 '\n',
	'\n',Ts2,V21,
	 '',%Ts,'],[',
	 %S5,
	 %Ts,
	 ')',',',S3],S2));

	(List2=[[n,"->"],[If,Then]]-> % if then else
	(N2 is N+1,
	pp_lp2p2([If],'',S4,N2),
	pp_lp2p2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'(','','\n',
	Ts,'',
	 S4,'->',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,'\n',Ts,
	 ')',',',S3],S2));
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',',S3],S2));
	
	(List2=[[n,code],Code1]-> % if then else
	(N2 is N+1,
	(%trace,
	(Code1=[A|_],not(predicate_or_rule_name(A)))->
	Code1=Code;[Code1]=Code),
	pp_lp2p2(Code,'',S4,N2),
	%pp_lp2p2([Then],'',S5,N2),
	%pp_lp2p2([Else],'',S51,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'{',%',','\n',Ts,'[',
	 S4,%'->',%'\n',Ts,
	 '\n'%Ts,'],[',
	 %S5,';',%'\n',Ts,
	 %'\n',%Ts,'],[',
	 %S51%,'\n'
	 ,Ts
	 ,'}',',',S3],S2));
	 
	(List2=[[n,"->"],[If,Then,Else]]-> % if then else
	(N2 is N+1,
	pp_lp2p2([If],'',S4,N2),
	pp_lp2p2([Then],'',S5,N2),
	pp_lp2p2([Else],'',S51,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	pp_lp2p2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'(',%',','\n',Ts,'[',
	 S4,'->',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,';',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S51%,'\n'
	 %,Ts
	 ,')',',',S3],S2));	 
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',','\n',Ts,S51,',',S3],S2));
	(%pp_lp2p2(List2,'',List2a,N),
	%trace,
	(List2=[[N_or_v,cut]|_],(N_or_v=n->true;N_or_v=v))->
	(pp_lp2p2(Lists3,'',S3,N),
	%pp4_lp2p3_1_4(List2,List2a),
	List2a="!",
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	(S3=''->Comma='';Comma=','),
	concat_list([S1,'\n',Ts,List2a,Comma,S3],S2));

	(%pp_lp2p2(List2,'',List2a,N),
	%trace,
	(List2=[[N_or_v,_]|_],(N_or_v=n->true;N_or_v=v))->
	(pp_lp2p2(Lists3,'',S3,N),
	pp4_lp2p3_1_4(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	(S3=''->Comma='';Comma=','),
	concat_list([S1,'\n',Ts,List2a,Comma,S3],S2));
	
	(N2 is N+1,
	pp_lp2p2(List2,'',List2a,N2),
	pp_lp2p2(Lists3,'',S3,N),
	%term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,%'\n',Ts,
	'\n',Ts,'(',
	List2a,
	'\n',Ts,')',
	',',S3
	],S2)))))))),
	%concat_list([S1,%'\n',Ts,
	%List2a,',',S3],S2)))),

/*	

	(term_to_atom(List2,List2a),
	pp_lp2p2(Lists3,'',S3,N),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,'\n',Ts,List2a,',',S3],S2)))),
	*/
	!.

pp_lp2p_1(List,String) :-
	pp4_lp2p3_1_4(List,Atom),
	atom_string(Atom,String).

pp_lp2p3(List1,S1,S3) :-
	%symbol_1_lp2p(Symbol,Symbol1),
	%List1=List2,
	((pp4_lp2p3_1(List1,String),
	%trace,
	%pp_lp2p2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([S1,String,".\n"%,'.\n\n'
	],S3)
	
	)->true;
	((pp4_lp2p3_2(List1,String),

	%trace,
	%pp_lp2p2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([S1,String,".\n"%,%B1,
	%'.\n\n'
	],S3))->true;
	
	
	%)->true;
	((pp4_lp2p3_3(List1,String,B1),

	%string_concat(B1,",",B11),
	concat_list([S1,String,B1,'.\n\n'],S3)->true;

	((pp4_lp2p3_4(List1,String,B1),
	%string_concat(B1,",",B11),
	concat_list([S1,String,B1,'.\n\n'],S3))))))),!.

pp4_lp2p3_1_4(L,S) :-
	(pp4_lp2p3_1(L,S)->true;
	(pp4_lp2p3_2(L,S)->true;
	((pp4_lp2p3_3(L,String,B1),
	string_concat(String,B1,S))->true;
	(pp4_lp2p3_4(L,String,B1),
	string_concat(String,B1,S))))),!.
	
pp4_lp2p3_1(List1,S3) :-
	List1=[[_N10,Name]],
	%term_to_atom(List2,List2a),
	%concat_list([S1,List2a,'\n'],S3)
	
	
	%term_to_atom(Variables1,Variables2),
	concat_list(['',Name%,'\n'
	],String),
	%trace,
	%pp_lp2p2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([String%,'\n\n'
	],S3).
	
pp4_lp2p3_2(List1,String) :-
	List1=[[_N10,Name],
		Variables],
	%term_to_atom(List2,List2a),
	%concat_list([S1,List2a,'\n'],S3)
	
	
	interpretstatementlp2p5(Variables,'',Variables2,false),
	concat_list(['',Name,'(',Variables2,')'%,
	%',',%Symbol1,
	%'\n'
	],String).
	
pp4_lp2p3_3(List2,String,B1) :-
	List2=[[_N1,Name],Variables1,Symbol,Body],
	symbol_1_lp2p(Symbol,Symbol1),
	interpretstatementlp2p5(Variables1,'',Variables2,false),
	concat_list(['',Name,'(',Variables2,')',
	'',Symbol1,'\n'],String),
	%trace,
	pp_lp2p2(Body,'',B1,1).
	%string_concat(B10).

pp4_lp2p3_4(List2,String,B1) :-
	List2=[[_N1,Name],Symbol,Body],
	symbol_1_lp2p(Symbol,Symbol1),
	%term_to_atom(Variables1,Variables2),
	concat_list([Name,Symbol1,'\n'],String),
	%trace,
	pp_lp2p2(Body,'',B1,1).
