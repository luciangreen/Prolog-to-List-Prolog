# Prolog-to-List-Prolog
Converts Prolog algorithms to List Prolog algorithms

# Prerequisites

* Use a search engine to find the Homebrew (or other) Terminal install command for your platform and install it, and search for the Terminal command to install swipl using Homebrew and install it or download and install SWI-Prolog for your machine at <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a>.

# Mac, Linux and Windows (with Linux commands installed): Prepare to run swipl

* In Terminal settings (Mac), make Bash the default shell:

```
/bin/bash
```

* In Terminal, edit the text file `~/.bashrc` using the text editor Nano:

```
nano ~/.bashrc
```

* Add the following to the file `~/.bashrc`:

```
export PATH="$PATH:/opt/homebrew/bin/"
```

* Check if `usr/local/bin` exists

```
ls -ld /usr/local/bin
```

* Create the directory if missing

```
sudo mkdir -p /usr/local/bin
```

* Link to swipl in Terminal

```
sudo ln -s /opt/homebrew/bin/swipl /usr/local/bin/swipl
```

# 1. Install manually

Download <a href="http://github.com/luciangreen/Prolog-to-List-Prolog/">this repository</a> and the <a href="https://github.com/luciangreen/listprologinterpreter">List Prolog interpreter</a>.

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>:

```
mkdir GitHub
cd GitHub/
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","Prolog-to-List-Prolog").
../
halt.
```

# Running Prolog to List Prolog

* In Shell:
`cd Prolog-to-List-Prolog`
`swipl`
`['p2lpconverter.pl'].`

Run:

Convert Prolog code to List Prolog code by copying Prolog algorithm into `test1.pl` and running: `p2lpconverter(S1),pp0(S1,S2),writeln(S2).`

# Tests

* Run `p2lp_test(A,B)` or `p2lp_test1(N,B)` where N is the test number from <a href="https://github.com/luciangreen/Prolog-to-List-Prolog/blob/master/p2lpverify.pl">Prolog to List Prolog/p2lpverify.pl</a>.

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
