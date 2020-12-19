# Prolog-to-List-Prolog
Converts Prolog algorithms to List Prolog algorithms


# Installation from List Prolog Package Manager (LPPM)

* Optionally, you can install from LPPM by installing <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine, downloading the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>,
```
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
```
loading LPPM with `['lppm'].` then installing the package by running `lppm_install("luciangreen","Prolog-to-List-Prolog").`.

# Prolog to List Prolog

Install:
Load each file into SWI-Prolog using `['la_strings.pl'].` and `['p2lpconverter.pl'].`

Run:

Convert Prolog code to List Prolog code by copying Prolog algorithm into `test1.pl` and running: `p2lpconverter(S),pp0(S).`

e.g.
```
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
a(1.1).
a("dsf").
a('dd').
a(A):-findall(A,(hello(A)),B).
a([A]):-A is 1+1.
ef(G):-(h(I)->true;true),!.
a([A]).
a(A).

[
[[n,a],[[[v,a],[v,b],[v,c]]]],
[[n,a],[[[v,a],[v,b]]]],
[[n,a],[[[v,a],|,[v,c]]]],
[[n,a],[[[[v,a],[v,b]]]]],
[[n,a],[[v,a],[]]],
[[n,a],[[v,a],[[v,a]]]],
[[n,a],[[[[v,a]],|,[v,c]]]],
[[n,a],[[v,a],[[[v,a]],|,[v,c]]]],
[[n,a],[[v,a],[[[v,a],[v,b]],|,[[v,c],[v,d]]]]],
[[n,ba],[12]],
[[n,a],[1.1]],
[[n,a],["dsf"]],
[[n,a],['dd']],
[[[n,a]],[[v,a]],":-"
[
	[[n,findall],[[v,a],[[n,hello],[[v,a]]],[v,b]]]
]],
[[[n,a]],[[[v,a]]],":-"
[
	[[n,+],[1,1,a]]
]],
[[[n,ef]],[[v,g]],":-"
[
	[[n,->],[[[n,h],[[v,i]]],[[n,true]],[[n,true]]]],
	[[n,cut]]
]],
[[n,a],[[[v,a]]]],
[[n,a],[[v,a]]],
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

Output: ```
[[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],:-,[[[[h,*,*],[i]],->,true,or,true],!]]]
```

Pretty-print List Prolog algorithm by typing e.g.:
`pp0([[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],(:-),[[[[h,*,*],[i]],->,true,or,true],!]]]).`
Output:
```
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

