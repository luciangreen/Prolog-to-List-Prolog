%% la_strings.pl

use_module(library(pio)).
use_module(library(dcg/basics)).

% Handle discontiguous predicates from other files
:- discontiguous phrase_from_file_s/2.
:- discontiguous writeln1/1.

open_s(File,Mode,Stream) :-
	atom_string(File1,File),
	open(File1,Mode,Stream),!.

string_atom(String,Atom) :-
	atom_string(Atom,String),!.

phrase_from_file_s(string(Output), String) :-
	atom_string(String1,String),
	phrase_from_file(string(Output), String1),!.
	
writeln1(Term) :-
	term_to_atom(Term,Atom),
	writeln(Atom),!.
	