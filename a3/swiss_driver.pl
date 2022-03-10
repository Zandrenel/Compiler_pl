:-use_module(library(process)).

:- debug.



process_query(In_file, Pretty_file, Ugly_file) :-
	call(ugly(In_file,Ugly_file)),
	call(pretty(Ugly_file, Pretty_file)).


ugly(In_file,Ugly_file) :-
     format(atom(F),'ast_generation(~w,~w)',[In_file,
					     Ugly_file]),
     write(F),
    process_create(path('scryer-prolog'),
		   ['-g',F,'a3.pl'],
		   []).

pretty(Ugly_file, Pretty_file) :-
    process_create(path(python),
		   [
		       'prettify.py',
		       Ugly_file,
		       Pretty_file
		   ],
		   []).
