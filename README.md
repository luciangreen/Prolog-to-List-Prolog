# Prolog-to-List-Prolog
Converts Prolog algorithms to List Prolog algorithms


# Installation from List Prolog Package Manager (LPPM)

* Optionally, you can install from LPPM by installing <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine, downloading the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>,
```
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
```
loading LPPM with `['lppm'].` then installing the package by running `lppm_install("luciangreen","Prolog-to-List-Prolog").`.

Install:
Load each file into SWI-Prolog using `['la_strings.pl'].` and `['p2lpconverter.pl'].`

Run:

Convert Prolog code to List Prolog code by running: p2lpconverter.
Input:
a.
b(C,D).
ef(G):-(h(I)->true;true),!.

Note: [a,*,*] not a, which is Simple List Prolog. [a,*,*] is for inputting into CAWPS predicate dictionary.

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

