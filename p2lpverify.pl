%% p2lp_test(Total,Score).

%%:- use_module(library(time)).

%% Test cases, Debug=trace=on or off, NTotal=output=total cases, Score=output=result

p2lp_test(NTotal,Score) :- p2lp_test(0,NTotal,0,Score),!.
p2lp_test(NTotal,NTotal,Score,Score) :- 
 findall(A,(p2lp_test(B,C,D),A=p2lp_test(B,C,D)),Ns),length(Ns,NL),NL=NTotal, !.
p2lp_test(NTotal1,NTotal2,Score1,Score2) :-
	NTotal3 is NTotal1+1,
	p2lp_test(NTotal3,In,Out),
	((p2lpconverter([string,In],Result1),
	%writeln1([result1,Result1]),
	Out=Result1	
	)->(Score3 is Score1+1,writeln0([p2lp_test,NTotal3,passed]));(Score3=Score1,writeln0([p2lp_test,NTotal3,failed]))),
	writeln0(""),
	p2lp_test(NTotal3,NTotal2,Score3,Score2),!.

%% Test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

p2lp_test1(N,Passed) :-
	p2lp_test(N,In,Out),
	((p2lpconverter([string,In],Result1),
	%writeln1([result1,Result1]),
	Out=Result1
	)->(Passed=passed,writeln0([p2lp_test,N,passed]));(Passed=failed,writeln0([p2lp_test,N,failed]))),!.


p2lp_test(1,":-use_module(library(date)).",
[
[":-",[n,use_module],[[[n,library],[date]]]]
]
).

p2lp_test(2,":-include('luciancicd.pl').",
[
[":-",[n,include],['luciancicd.pl']]
]
).

p2lp_test(3,":-include('../Prolog-to-List-Prolog/p2lpconverter.pl').",
[
[":-",[n,include],['../Prolog-to-List-Prolog/p2lpconverter.pl']]
]
).

p2lp_test(4,"use_module(library(pio)).",
[
[[n,use_module],[[[n,library],[pio]]]]
]
).

p2lp_test(5,"use_module(library(dcg/basics)).",
[
[[n,use_module],[[[n,library],['dcg/basics']]]]
]
).

p2lp_test(6,":-dynamic keep_comments/1.",
[
[":-",[n,dynamic],[keep_comments,"/",1]]
]
).

p2lp_test(7,"a:-a.",
[
[[n,a],":-",
[
	[[n,a]]
]]
]
).

p2lp_test(8,"a(A,B).
/*1*/
%2",
[
[[n,a],[[v,a],[v,b]]],
[[n,comment],["/*1*/"]],
[[n,comment],["%2"]]
]
).

p2lp_test(9,"a:-d(E).
/*2*/
/*3*/",
[[[n, a], ":-", [[[n, d], [[v, e]]]]], 
[[n, comment], ["/*2*/"]], 
[[n, comment], ["/*3*/"]]]


).

p2lp_test(10,"a(B,C)-->d(E).
/*1*/
/*2*/",
[
[[n,a],[[v,b],[v,c]],"->",
[
	[[n,d],[[v,e]]]
]],
[[n,comment],["/*1*/"]],
[[n,comment],["/*2*/"]]
]
).

p2lp_test(11,"a:-d(E).
/*2*/
/*3*/",
[
[[n,a],":-",
[
	[[n,d],[[v,e]]]
]],
[[n,comment],["/*2*/"]],
[[n,comment],["/*3*/"]]
]
).

p2lp_test(12,"a-->d(E).
/*2*/
/*3*/",
[
[[n,a],"->",
[
	[[n,d],[[v,e]]]
]],
[[n,comment],["/*2*/"]],
[[n,comment],["/*3*/"]]
]
).

p2lp_test(13,"a-->a.",
[
[[n,a],"->",
[
	[[n,a]]
]]
]
).

p2lp_test(14,"a-->d(E).
/*2*/
/*3*/",
[
[[n,a],"->",
[
	[[n,d],[[v,e]]]
]],
[[n,comment],["/*2*/"]],
[[n,comment],["/*3*/"]]
]
).

p2lp_test(15,"a:-b([A,B],1,[C]).",
[
[[n,a],":-",
[
	[[n,b],[[[v,a],[v,b]],1,[[v,c]]]]
]]
]
).

p2lp_test(16,"a.
/*1*/",
[
[[n,a]],
[[n,comment],["/*1*/"]]
]
).

p2lp_test(17,"b:-c.",
[
[[n,b],":-",
[
	[[n,c]]
]]
]
).

p2lp_test(18,"a:-b,c,d.
/*1*/
%2",
[
[[n,a],":-",
[
	[[n,b]],
	[[n,c]],
	[[n,d]]
]],
[[n,comment],["/*1*/"]],
[[n,comment],["%2"]]
]
).

p2lp_test(19,"a(A):-[]=[[]].",
[
[[n,a],[[v,a]],":-",
[
	[[n,equals4],[[],[[]]]]
]]
]
).

p2lp_test(20,"a(A):-([]=[[]]).",
[
[[n,a],[[v,a]],":-",
[
	[
	[[n,equals4],[[],[[]]]]
	]
]]
]
).

p2lp_test(21,"a(A):-([1]=A).",
[
[[n,a],[[v,a]],":-",
[
	[
	[[n,equals4],[[1],[v,a]]]
	]
]]
]
).

p2lp_test(22,"a(A):-(B=A).",
[
[[n,a],[[v,a]],":-",
[
	[
	[[n,=],[[v,b],[v,a]]]
	]
]]
]
).

p2lp_test(23,"a(A):-([1]=[1]).",
[
[[n,a],[[v,a]],":-",
[
	[
	[[n,equals4],[[1],[1]]]
	]
]]
]
).

p2lp_test(24,"a([A,B,C]).",
[
[[n,a],[[[v,a],[v,b],[v,c]]]]
]
).

p2lp_test(25,"a([A,B]).",
[
[[n,a],[[[v,a],[v,b]]]]
]
).

p2lp_test(26,"a([A|C]).",
[
[[n,a],[[[v,a],"|",[v,c]]]]
]
).

p2lp_test(27,"a([[A,B]]).",
[
[[n,a],[[[[v,a],[v,b]]]]]
]
).

p2lp_test(28,"a(A,[]).",
[
[[n,a],[[v,a],[]]]
]
).

p2lp_test(29,"a(A,[A]).",
[
[[n,a],[[v,a],[[v,a]]]]
]
).

p2lp_test(30,"a([[A]|C]).",
[
[[n,a],[[[[v,a]],"|",[v,c]]]]
]
).

p2lp_test(31,"a(A,[[A]|C]).",
[
[[n,a],[[v,a],[[[v,a]],"|",[v,c]]]]
]
).

p2lp_test(32,"a(A,[[A,B]|[C,D]]).",
[
[[n,a],[[v,a],[[[v,a],[v,b]],"|",[[v,c],[v,d]]]]]
]
).

p2lp_test(33,"ba(12).",
[
[[n,ba],[12]]
]
).

p2lp_test(34,"ba(12,1).",
[
[[n,ba],[12,1]]
]
).

p2lp_test(35,"a(1.1).",
[
[[n,a],[1.1]]
]
).

p2lp_test(36,"a(\"dsf\").",
[
[[n,a],["dsf"]]
]
).

p2lp_test(37,"a(dd).",
[
[[n,a],['dd']]
]
).

p2lp_test(38,"a(A):-findall(A,hello(A),B).",
[
[[n,a],[[v,a]],":-",
[
	[[n,findall],
	[
		[v,a],

		[[n,hello],[[v,a]]],

		[v,b]
	]]
]]
]
).

p2lp_test(39,"a(A):-findall(A,hello(A),B),!.",
[
[[n,a],[[v,a]],":-",
[
	[[n,findall],
	[
		[v,a],

		[[n,hello],[[v,a]]],

		[v,b]
	]],
	[[n,cut]]
]]
]
).

p2lp_test(40,"a(A):-findall(A,(hello(A),hello(A)),B).",
[
[[n,a],[[v,a]],":-",
[
	[[n,findall],
	[
		[v,a],

		[
		[[n,hello],[[v,a]]],
		[[n,hello],[[v,a]]]
		],

		[v,b]
	]]
]]
]
).

p2lp_test(41,"a([A]):-A is 1+1.",
[
[[n,a],[[[v,a]]],":-",
[
	[[n,+],[1,1,[v,a]]]
]]
]
).

p2lp_test(42,"ef(G):-(h(I)->true;true).",
[
[[n,ef],[[v,g]],":-",
[
	[[n,"->"],
	[
		[[n,h],[[v,i]]],

		[[n,true]],

		[[n,true]]
	]]
]]
]
).

p2lp_test(43,"ef(G):-(h(I)->true;true),!.",
[
[[n,ef],[[v,g]],":-",
[
	[[n,"->"],
	[
		[[n,h],[[v,i]]],

		[[n,true]],

		[[n,true]]
	]],
	[[n,cut]]
]]
]
).

p2lp_test(44,"ef(G):-(h(I)->true).",
[
[[n,ef],[[v,g]],":-",
[
	[[n,"->"],
	[
		[[n,h],[[v,i]]],

		[[n,true]]
	]]
]]
]
).

p2lp_test(45,"ef(G):-(h(I)->true),!.",
[
[[n,ef],[[v,g]],":-",
[
	[[n,"->"],
	[
		[[n,h],[[v,i]]],

		[[n,true]]
	]],
	[[n,cut]]
]]
]
).

p2lp_test(46,"compound21(T,U)-->item(I).",
[
[[n,compound21],[[v,t],[v,u]],"->",
[
	[[n,item],[[v,i]]]
]]
]
).

p2lp_test(47,"compound21(T,U)-->item(I),!.",
[
[[n,compound21],[[v,t],[v,u]],"->",
[
	[[n,item],[[v,i]]],
	[[n,cut]]
]]
]
).

p2lp_test(48,"a([A]).",
[
[[n,a],[[[v,a]]]]
]
).

p2lp_test(49,"a(A).",
[
[[n,a],[[v,a]]]
]
).

p2lp_test(50,"compound21(T,U)-->{wrap(I,Itemname1)}.",
[
[[n,compound21],[[v,t],[v,u]],"->",
[
	[[n,code],[[n,wrap],[[v,i],[v,itemname1]]]]
]]
]
).

p2lp_test(51,"compound21(T,U)-->{wrap(I,Itemname1),append(T,Itemname1,V)}.",
[
[[n,compound21],[[v,t],[v,u]],"->",
[
	[[n,code],[[[n,wrap],[[v,i],[v,itemname1]]],[[n,append],[[v,t],[v,itemname1],[v,v]]]]]
]]
]
).

p2lp_test(52,"compound21(T,U)-->item(I),lookahead(\"]\"),{wrap(I,Itemname1),append(T,Itemname1,V)},compound212(V,U).",
[
[[n,compound21],[[v,t],[v,u]],"->",
[
	[[n,item],[[v,i]]],
	[[n,lookahead],["]"]],
	[[n,code],[[[n,wrap],[[v,i],[v,itemname1]]],[[n,append],[[v,t],[v,itemname1],[v,v]]]]],
	[[n,compound212],[[v,v],[v,u]]]
]]
]
).

p2lp_test(53,"a(A):-findall([A,C],hello(A,C),B).",
[
[[n,a],[[v,a]],":-",
[
	[[n,findall],
	[
		[[v,a],[v,c]],

		[[n,hello],[[v,a],[v,c]]],

		[v,b]
	]]
]]
]
).

p2lp_test(54,"a:-a(\"\n\",\"\\\"\").",
[[[n, a], ":-",[[[n,a],["\n",""""]]]]]
).

p2lp_test(55,"a:-a(\"\\\"\").",
[[[n, a], ":-",[[[n,a],[""""]]]]]
).

p2lp_test(56,"a:-a(\" \\\"\").",
[[[n, a], ":-",[[[n,a],[" \""]]]]]
).

p2lp_test(57,"a:-a(\"\\\" \").",
[[[n, a], ":-",[[[n,a],[""" "]]]]]
).
