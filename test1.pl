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
a(A):-findall(A,(hello(A),hello(A)),B).
a([A]):-A is 1+1.
ef(G):-(h(I)->true;true),!.
a([A]).
a(A).
compound21(T,U)->item(I),lookahead("]"),{wrap(I,Itemname1),append(T,Itemname1,V)},compound212(V,U).
checktypes_inputs(Function,Vars1):-(types(on)->(typestatements(TypeStatements1),modestatements(ModeStatements1),checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1));true),!.
checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1):-length(Vars1,L),L is 0,Vars1=[],get_lang_word("input type check",Input_type_check),(types(on)->debug_types_call([Function,"/","~",L,Input_type_check]);true),(types(on)->debug_call(Skip,[Function,Vars1]);true),(types(on)->debug_exit(Skip,[Function,Vars1]);true),(types(on)->(debug_types_exit([Function,"/","~",L,Input_type_check]));true),!.
checktypes0_inputs(Function,Vars1,TypeStatements1,ModeStatements1):-length(Vars1,L),get_lang_word("input type check",Input_type_check),(types(on)->(debug_types_call([Function,"/","~",L,Input_type_check]));true),(member([Function|[TypeStatements2]],TypeStatements1),member([Function|[ModeStatements2]],ModeStatements1),extract_modes1(TypeStatements2,TypeStatements3,Vars1,Vars2,ModeStatements2),(types(on)->debug_call(Skip,[Function,Vars2]);true),((checktypes1(Vars2,TypeStatements3,TypeStatements3,TypeStatements1))->((types(on)->debug_exit(Skip,[Function,Vars2]);true),(types(on)->(debug_types_exit([Function,"/","~",L,Input_type_check]));true));((types(on)->debug_fail(Skip,[Function,Vars1]);true),(types(on)->(debug_types_fail([Function,"/","~",L,Input_type_check]));true)))),!.
extract_modes1(TypeStatements1,TypeStatements3,Vars1,Vars2,ModeStatements1):-extract_modes2(TypeStatements1,[],TypeStatements3,Vars1,[],Vars2,ModeStatements1),!.
extract_modes2([],TypeStatements2a,TypeStatements2a,[],Vars,Vars,[]):-!.
extract_modes2(TypeStatements1,TypeStatements2a,TypeStatements3,Vars1,Vars2,Vars3,ModeStatements1):-get_lang_word("input",Input),ModeStatements1=[Input|ModeStatements3],TypeStatements1=[TypeStatements2|TypeStatements3a],Vars1=[Vars11|Vars12],append(TypeStatements2a,[TypeStatements2],TypeStatements4),append(Vars2,[Vars11],Vars4),extract_modes2(TypeStatements3a,TypeStatements4,TypeStatements3,Vars12,Vars4,Vars3,ModeStatements3),!.
extract_modes2(TypeStatements1,TypeStatements2a,TypeStatements3,Vars1,Vars2,Vars3,ModeStatements1):-get_lang_word("output",Output),ModeStatements1=[Output|ModeStatements3],TypeStatements1=[TypeStatements2|TypeStatements3a],Vars1=[_Vars11|Vars12],extract_modes2(TypeStatements3a,TypeStatements2a,TypeStatements3,Vars12,Vars2,Vars3,ModeStatements3),!.
checktypes(Function,Vars1):-(types(on)->(typestatements(TypeStatements1),checktypes0(Function,Vars1,TypeStatements1));true),!.
checktypes0(Function,Vars1,TypeStatements1):-get_lang_word("Type check",Type_check),length(Vars1,L),L is 0,Vars1=[],(types(on)->(debug_types_call([Function,"/",L,Type_check]));true),(types(on)->debug_call(Skip,[Function,Vars1]);true),(types(on)->debug_exit(Skip,[Function,Vars1]);true),(types(on)->(debug_types_exit([Function,"/",L,Type_check]));true),!.
checktypes0(Function,Vars1,TypeStatements1):-get_lang_word("Type check",Type_check),length(Vars1,L),(types(on)->(debug_types_call([Function,"/",L,Type_check]));true),(types(on)->debug_call(Skip,[Function,Vars1]);true),((member([Function|[TypeStatements2]],TypeStatements1),checktypes1(Vars1,TypeStatements2,TypeStatements2,TypeStatements1))->((types(on)->debug_exit(Skip,[Function,Vars1]);true),(types(on)->(debug_types_exit([Function,"/",L,Type_check]));true));((types(on)->debug_fail(Skip,[Function,Vars1]);true),(types(on)->(debug_types_fail([Function,"/",L,Type_check]));true))),!.
checktypes1([],[],U1,U2):-!.
a(A):-TypeStatements1=[[[T,Dbw_list]|[TypeStatements3]]|TypeStatements4a],(types(on)->(debug_call(Skip,[[T,Dbw_list],TypeStatements3]));true).
checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4):-get_lang_word("t",T),get_lang_word("list",Dbw_list),Vars1=[Vars2|Vars3],list1(Vars2,Undef1,Undef2),TypeStatements1=[[[T,Dbw_list]|[TypeStatements3]]|TypeStatements4a],(types(on)->(debug_call(Skip,[[T,Dbw_list],TypeStatements3]));true),((checktypes3(Vars2,TypeStatements3,TypeStatements2,TypeStatements4))->((types(on)->(debug_exit(Skip,[[T,Dbw_list],Vars2]));true),checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4));(types(on)->(debug_fail(Skip,[[T,Dbw_list],Vars2]));true)).
checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4):-get_lang_word("t",T),get_lang_word("list",Dbw_list),TypeStatements1=[[[T,Dbw_list]|[TypeStatements3]]|TypeStatements4a],(types(on)->(debug_call(Skip,[[T,Dbw_list],TypeStatements3]));true),((checktypes3(Vars1,TypeStatements3,TypeStatements2,TypeStatements4))->(types(on)->debug_exit(Skip,[[T,Dbw_list],Vars1]);true);(types(on)->debug_fail(Skip,[[T,Dbw_list],Vars1]);true)).
checktypes1(Vars1,TypeStatements1,TypeStatements2,TypeStatements4):-get_lang_word("t",T),get_lang_word("brackets",Dbw_brackets),TypeStatements1=[[[T,Dbw_brackets]|[TypeStatements3]]|TypeStatements4a],(types(on)->debug_call(Skip,[[T,Dbw_brackets],TypeStatements3]);true),((Vars1=[Vars2|Vars3],checktypes1(Vars2,TypeStatements3,TypeStatements2,TypeStatements4))->((types(on)->debug_exit(Skip,[[T,Dbw_brackets],Vars1]);true),checktypes1(Vars3,TypeStatements4a,TypeStatements2,TypeStatements4));(types(on)->debug_fail(Skip,[[T,Dbw_brackets],Vars1]);true)),!.
checktypes1(Vars1,TypeStatements0,TypeStatements1,TypeStatements4):-Vars1=[Vars2|Vars3],TypeStatements0=[TypeStatements2|TypeStatements3],checktypes2(Vars2,TypeStatements2,TypeStatements1,TypeStatements4),checktypes1(Vars3,TypeStatements3,TypeStatements1,TypeStatements4).
checktypes2(Vars,TypeStatements1,TypeStatements2,C):-get_lang_word("t",T),get_lang_word("number",Dbw_number),TypeStatements1=[T,Dbw_number],(types(on)->debug_call(Skip,[[T,Dbw_number],Vars]);true),((number(Vars))->(types(on)->debug_exit(Skip,[[T,Dbw_number],Vars]);true);(types(on)->debug_fail(Skip,[[T,Dbw_number],Vars]);true)).
checktypes2(Vars,TypeStatements1,TypeStatements2,U1):-get_lang_word("t",T),get_lang_word("predicatename",Dbw_predicatename),get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,TypeStatements1=[T,Dbw_predicatename],(types(on)->debug_call(Skip,[[T,Dbw_predicatename],Vars]);true),((Vars=[Dbw_n,U2])->(types(on)->debug_exit(Skip,[[T,Dbw_predicatename],Vars]);true);(types(on)->debug_fail(Skip,[[T,Dbw_predicatename],Vars]);true)).
checktypes2(Vars,TypeStatements1,TypeStatements2,U1):-get_lang_word("t",T),get_lang_word("string",Dbw_string),TypeStatements1=[T,Dbw_string],(types(on)->debug_call(Skip,[[T,Dbw_string],Vars]);true),((string(Vars))->(types(on)->debug_exit(Skip,[[T,Dbw_string],Vars]);true);(types(on)->debug_fail(Skip,[[T,Dbw_string],Vars]);true)).
checktypes2(Vars,TypeStatements1,TypeStatements2,U1):-get_lang_word("t",T),get_lang_word("any",Dbw_any),TypeStatements1=[T,Dbw_any],(types(on)->debug_call(Skip,[[T,Dbw_any],Vars]);true),((true)->(types(on)->debug_exit(Skip,[[T,Dbw_any],Vars]);true);(types(on)->debug_fail(Skip,[[T,Dbw_any],Vars]);true)).
checktypes2(Vars,TypeStatements1,TypeStatements2,TypeStatements4):-get_lang_word("t",T),get_lang_word("list",Dbw_list),get_lang_word("brackets",Dbw_brackets),get_lang_word("number",Dbw_number),get_lang_word("predicatename",Dbw_predicatename),get_lang_word("string",Dbw_string),get_lang_word("any",Dbw_any),TypeStatements1=[T,Type],(not(Type=Dbw_list),not(Type=Dbw_brackets),not(Type=Dbw_number),not(Type=Dbw_predicatename),not(Type=Dbw_string),not(Type=Dbw_any)),(types(on)->debug_call(Skip,[[T,Type],Vars]);true),((member([[T,Type]|[TypeStatements3]],TypeStatements4),(checktypes1(Vars,TypeStatements3,TypeStatements2,TypeStatements4)->true;checktypes1([Vars],TypeStatements3,TypeStatements2,TypeStatements4)))->(types(on)->debug_exit(Skip,[[T,Type],Vars]);true);(types(on)->debug_fail(Skip,[[T,Type],Vars]);true)).
checktypes3([],U1,TypeStatements2,U2):-!.
checktypes3(Vars,TypeStatements3,TypeStatements2,TypeStatements6):-length(TypeStatements3,L),length(L1,L),append(L1,L2,Vars),checktypes1(L1,TypeStatements3,TypeStatements2,TypeStatements6),checktypes3(L2,TypeStatements3,TypeStatements2,TypeStatements6),!.