# Prolog-to-List-Prolog
Converts Prolog algorithms to List Prolog algorithms

# Prerequisites

* Please download and install SWI-Prolog for your machine at `https://www.swi-prolog.org/build/`.

# 1. Install manually

Download <a href="http://github.com/luciangreen/Prolog-to-List-Prolog/">this repository</a> and the <a href="https://github.com/luciangreen/listprologinterpreter">List Prolog interpreter</a>.

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>:

```
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","Prolog-to-List-Prolog").
halt
```

# Running Prolog to List Prolog

* In Shell:
`cd Prolog-to-List-Prolog`
`swipl`
`['p2lpconverter.pl'].`

Run:

Convert Prolog code to List Prolog code by copying Prolog algorithm into `test1.pl` and running: `p2lpconverter(S1),pp0(S1,S2),writeln(S2).`

e.g.
```
a:-a.
a->a.
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
a(A):-findall(A,(hello(A),hello(A)),B).
a([A]):-A is 1+1.
ef(G):-(h(I)->true;true),!.
a([A]).
a(A).
compound21(T,U)->item(I),lookahead("]"),{wrap(I,Itemname1),append(T,Itemname1,V)},compound212(V,U).
a(A):-findall([A,C],hello(A,C),B).

[
[[n,a],":-",
[
	[[n,a]]
]],
[[n,a],"->",
[
	[[n,a]]
]],
[[n,a],[[v,a]],":-",
[
	[[n,equals4],[[],[[]]]]
]],
[[n,a],[[v,a]],":-",
[
	[
	[[n,equals4],[[],[[]]]]
	]
]],
[[n,a],[[v,a]],":-",
[
	[
	[[n,equals4],[[1],[v,a]]]
	]
]],
[[n,a],[[v,a]],":-",
[
	[
	[[n,=],[[v,b],[v,a]]]
	]
]],
[[n,a],[[v,a]],":-",
[
	[
	[[n,equals4],[[1],[1]]]
	]
]],
[[n,a],[[[v,a],[v,b],[v,c]]]],
[[n,a],[[[v,a],[v,b]]]],
[[n,a],[[[v,a],"|",[v,c]]]],
[[n,a],[[[[v,a],[v,b]]]]],
[[n,a],[[v,a],[]]],
[[n,a],[[v,a],[[v,a]]]],
[[n,a],[[[[v,a]],"|",[v,c]]]],
[[n,a],[[v,a],[[[v,a]],"|",[v,c]]]],
[[n,a],[[v,a],[[[v,a],[v,b]],"|",[[v,c],[v,d]]]]],
[[n,ba],[12]],
[[n,ba],[12,1]],
[[n,a],[1.1]],
[[n,a],["dsf"]],
[[n,a],[dd]],
[[n,a],[[v,a]],":-",
[
	[[n,findall],
	[
		[v,a],

		[[n,hello],[[v,a]]],

		[v,b]
	]]
]],
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
]],
[[n,a],[[[v,a]]],":-",
[
	[[n,+],[1,1,[v,a]]]
]],
[[n,ef],[[v,g]],":-",
[
	[[n,"->"],
	[
		[[n,h],[[v,i]]],

		[[n,true]],

		[[n,true]]
	]],
	[[n,cut]]
]],
[[n,a],[[[v,a]]]],
[[n,a],[[v,a]]],
[[n,compound21],[[v,t],[v,u]],"->",
[
	[[n,item],[[v,i]]],
	[[n,lookahead],["]"]],
	[[n,code],[[[n,wrap],[[v,i],[v,itemname1]]],[[n,append],[[v,t],[v,itemname1],[v,v]]]]],
	[[n,compound212],[[v,v],[v,u]]]
]],
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
```

# Prolog to Simple List Prolog

Install:
Load each file into SWI-Prolog using `['la_strings.pl'].` and `['p2slpconverter.pl'].`

Run:

Convert Prolog code to List Prolog code by copying Prolog algorithm into test1.pl and running: `p2slpconverter.`
Input:
```
a.
b(C,D).
ef(G):-(h(I)->true;true),!.
```
Note: `[a,*,*]` not `a`, which is Simple List Prolog. `[a,*,*]` is for inputting into CAWPS predicate dictionary.

Output: 
```
[[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],:-,[[[[h,*,*],[i]],->,true,or,true],!]]]
```

Pretty-print List Prolog algorithm by typing e.g.:
```
pp0([[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],(:-),[[[[h,*,*],[i]],->,true,or,true],!]]]).`
Output:

[
[[a,*,*]],
[[b,*,*],[c,d]],
[[ef,*,*],[g],(:-),
[
	[[[h,*,*],[i]],->,true,or,true],
	!
]]
]
```

* See also <a href="https://github.com/luciangreen/State-Machine-to-List-Prolog">State Machine to List Prolog</a>, which converts from the State Machine generated within SSI (a Prolog compiler written in Prolog) to List Prolog (a version of Prolog with algorithms written as lists).  The State Machine may be operated on by, for example, optimisers.
