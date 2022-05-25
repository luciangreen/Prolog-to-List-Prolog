pretty_print(List,String) :-
	(pp0(List,String)->true;
	pp_1(List,String)).

symbol_1(":-","\":-\"").
symbol_1("->","\"->\"").

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
pp1([List1],S1,S2) :-
	%List1=[List2],
	pp3(List1,S1,S2).
pp1(List1,S1,S2) :-
	List1=[List2|Lists3],
	pp3(List2,S1,S3),
		pp1(Lists3,S3,S2),!.
pp2([],S,S,_N):-!.
pp2(List1,S1,S2,N) :-
	List1=[List2],
	
	(List2=[[n,"->"],[If,Then]]-> % if then else
	(N2 is N+1,
	pp2([If],'',S4,N2),
	pp2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	%pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,"->"]','\n',Ts,'[',
	 S4,',',%'\n',Ts,
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
	concat_list([S1,'\n',Ts,'[[n,"->"]','\n',Ts,'[',
	 S4,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,',',%'\n',Ts,
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
	],S2)))),
	!.
pp2(List1,S1,S2,N) :-
	List1=[List2|Lists3],
	%write("\t"),write(List2),writeln(","),
	(List2=[[n,"->"],[If,Then]]-> % if then else
	(N2 is N+1,
	pp2([If],'',S4,N2),
	pp2([Then],'',S5,N2),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	pp2(Lists3,'',S3,N),
	concat_list([S1,'\n',Ts,'[[n,"->"]','\n',Ts,'[',
	 S4,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,'\n',Ts,']]',',',S3],S2));
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
	concat_list([S1,'\n',Ts,'[[n,"->"]','\n',Ts,'[',
	 S4,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S5,',',%'\n',Ts,
	 '\n',%Ts,'],[',
	 S51,'\n',Ts,']]',',',S3],S2));
	%concat_list([S1,'\n',Ts,S4,',','\n',Ts,S5,',','\n',Ts,S51,',',S3],S2));
	(%pp2(List2,'',List2a,N),
	List2=[[n,_]|_],
	pp2(Lists3,'',S3,N),
	term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,'\n',Ts,List2a,',',S3],S2));
	
	(pp2(List2,'',List2a,N),
	pp2(Lists3,'',S3,N),
	%term_to_atom(List2,List2a),
	length(Counter,N),
	findall('\t',member(_,Counter),Ts1),
	concat_list(Ts1,Ts),
	concat_list([S1,%'\n',Ts,
	'\n',Ts,'[',
	List2a,
	'\n',Ts,']',
	',',S3
	],S2)))),
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
	symbol_1(Symbol,Symbol1),
	List1=List2,
	(((List2=[[_N10,_Name]]->true;List2=[[_N10,_Name],
		_Variables]),
	term_to_atom(List2,List2a),
	concat_list([S1,List2a,',\n'],S3))->true;
	((List2=[[N1,Name],Variables1,Symbol,Body]->
	(term_to_atom(Variables1,Variables2),
	concat_list([S1,'[[',N1,',',Name,'],',Variables2,
	',',Symbol1,',\n['],String),
	%trace,
	pp2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([String,B1,'\n]],\n'],S3)))->true;
	List2=[[N1,Name],Symbol,Body]->
	(%term_to_atom(Variables1,Variables2),
	concat_list([S1,'[[',N1,',',Name,'],',Symbol1,',\n['],String),
	%trace,
	pp2(Body,'',B1,1),
	%string_concat(B1,",",B11),
	concat_list([String,B1,'\n]],\n'],S3)))),!.
