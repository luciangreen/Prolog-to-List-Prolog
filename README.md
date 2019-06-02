# Prolog-to-List-Prolog
Converts Prolog algorithms to List Prolog algorithms

Install:
Load each file into SWI-Prolog using ['file']. (given file.pl)

Run:

Convert Prolog code to List Prolog code by running: p2lpconverter.
Input:
a.
b(C,D).
ef(G):-(h(I)->true;true),!.

Output: [[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],:-,[[[[h,*,*],[i]],->,true,or,true],!]]]

Pretty-print List Prolog algorithm by typing e.g.:
pp0([[[a,*,*]],[[b,*,*],[c,d]],[[ef,*,*],[g],(:-),[[[[h,*,*],[i]],->,true,or,true],!]]]).
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

