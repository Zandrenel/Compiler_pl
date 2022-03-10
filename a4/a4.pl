/*===========================================================
Author: Alexander De Laurentiis

===========================================================*/

:-use_module(library(files)).
:-use_module(library(charsio)).
:-use_module(library(clpz)).
:-use_module(library(dcgs)).
:-use_module(library(lists)).
:-use_module(library(iso_ext)).
:-use_module(library(os)).

% ?- '$shell'("python prettyfy.py",0).
%@ caught: error(existence_error(procedure,'$shell'/2),'$shell'/2)






% Operators, punctuation, and reserved words
% token(Token, Def) 
token("==", operator).
token("<>", operator).
token("<",  operator).
token(">",  operator).
token("<=", operator).
token(">=", operator).
token("+",  operator).
token("-",  operator).
token("*",  operator).
token("/",  operator).
token("=",  operator).
token("|",  operator).
token("&",  operator).
token("!",  operator).
token("(",  punctuation).
token(")",  punctuation).
token("{",  punctuation).
token("}",  punctuation).
token("[",  punctuation).
token("]",  punctuation).
token(";",  punctuation).
token(",",  punctuation).
token(".",  operator).
token(":",  operator).
token("::", operator).
token("->", operator).
token("if", reserved).
token("then", reserved).
token("else", reserved).
token("integer", reserved).
token("float", reserved).
token("void", reserved).
token("public", reserved).
token("private", reserved).
token("func", reserved).
token("var", reserved).
token("struct", reserved).
token("while", reserved).
token("func", reserved).
token("read", reserved).
token("write", reserved).
token("return", reserved).
token("self", reserved).
token("inherits", reserved).
token("let", reserved).
token("impl", reserved).
token(X, 'Float'):-float_(X).
token(X, 'ID'):-id(X).
token(X, 'Integer'):-integer_(X).
token("//", 'Inline Comment').
token("/*", 'Comment start').
token("*/", 'Comment end').


/*===========================================================
 File reading logic to read lines as characters and return a list of the
 characters with the line number  
===========================================================*/
stream_to_charlist(Stream,[]):-at_end_of_stream(Stream),!.
stream_to_charlist(Stream,[H|T]):-
    get_char(Stream,H),
    stream_to_charlist(Stream,T).


/*===========================================================
 File reading logic to read lines as characters and return a list of the
 characters with the line number  
===========================================================*/


analyze([],_,_,_).
analyze([H|T],L,Cl0,O) :-
    Cl0 #> 0,
    token(H,D),
    D = 'Comment start',
    Cl #= Cl0 + 1,
    Output = ['Comment',['level:',Cl],H,L],
    write(O,Output),
    analyze(T,L,Cl,O),!.
analyze([H|T],L,Cl0,O) :-
    Cl0 #> 0,
    token(H,D),
    (D = 'Comment end'
    -> Cl #= Cl0 - 1;
     Cl = Cl0),
    Output = ['Comment',['level:',Cl],H,L],
    write(O,Output),
    analyze(T,L,Cl,O),!.
analyze([H|T],L0,Cl,O) :-
    H = "//",
    split_delimiter([H|T],Comment,Rest,['\n']),
    Output = ['Comment',Comment,L0],
    write(O,Output),
    write("inline comment"),
    analyze(Rest,L0,Cl,O),!.
analyze([H|T],L,Cl0,O) :-
    token(H,D),
    (D = 'Comment start'
    -> Cl #= Cl0 + 1;
     Cl = Cl0),
    Output = [D,H,L],
    write(O,Output),
    analyze(T,L,Cl,O),!.
analyze([H|T],L0,Cl,O) :-
    H = ['\n'],
    L #= L0 + 1,
    write(O,'\n'),
    analyze(T,L,Cl,O),!.
analyze([H|T],L,Cl,O) :-
    Output = ['invalidtoken',H,L],
    write(O,Output),
    analyze(T,L,Cl,O),!.
analyze([_H|T],L,O) :-
    analyze(T,L,Cl,O),!.


/*===========================================================

The procedures to test each of the given positive and negative
test case files along with one that will allow the specification
of the input and output fil names.

===========================================================*/

test_negative :-
    file_exists("lexnegativegrading.src"),
    open("lexnegativegrading.src",read,S1,[]),
    open("lexnegativegradingout.txt",write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst),
    analyze(Lst,0,0,S2),
    close(S1),
    close(S2).


test_positive :-
    file_exists("lexpositivegrading.src"),
    open("lexpositivegrading.src",read,S1,[]),
    open("lexpositivegradingout.txt",write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst),
    analyze(Lst,0,0,S2),
    close(S1),
    close(S2).

analyze_file(Filein,Fileout) :-
    open(Filein,read,S1,[]),
    open(Fileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst),
    write(Lst),
    analyze(Lst,0,0,S2),
    write("apples are great"),
    close(S1),
    close(S2).

/*===========================================================

The execution functions for usage of ediprolog in emacs, the 
best way in which to program in scryer-prolog.

===========================================================*/


% ?- test_negative.
%@ false.
%@    true
%@ ;  false.

% ?- test_positive.
%@ false.
%@    true
%@ ;  false.


% ?- analyze_file("studenttestcases.src","studenttestcasesout.txt").
%@ ["//","//","reserved","word","tests","and","operators","and","symbols","
%@ ","==","+","|","(",";","if","public","read","
%@ ","<",">","-","&",")",",","then","private","write","
%@ ","<","*","!","{",".","else","func","return","
%@ ",">","/","}",":","integer","var","self","
%@ ","<=","=","[",":",":","float","struct","inherits","
%@ ",">=","]","->","void","while","let","
%@ ","func","impl","
%@ ","
%@ ","
%@ ","
%@ ","
%@ ","//","int","tests","
%@ ","0","
%@ ","1","
%@ ","01","
%@ ","10","
%@ ","12.","
%@ ","000000000123","
%@ ","12345000000000001","
%@ ","
%@ ","
%@ ","//","float","tests","
%@ ","01.23","
%@ ","1.2323232320","
%@ ","1.232323232","
%@ ","12.34","
%@ ","120.34e10","
%@ ","12345.6789e","-","123","
%@ ","0000001.232323232","
%@ ","2200000000012.34","
%@ ","120.34e10","
%@ ","12345.6789e","-","123","
%@ ","12345.6789e","+","123","
%@ ","12345.6789e","-","0123","
%@ ","12345.6789e","+","1230","
%@ ","
%@ ","
%@ ","
%@ ","//","id","tests","
%@ ","abc","
%@ ","abc1","
%@ ","a1bc","
%@ ","abc_1abc","
%@ ","abc1_abc","
%@ ","
%@ ","//","this","is","an","inline","comment","
%@ ","
%@ ","/*","this","is","a","single","line","block","comment","*/","
%@ ","
%@ ","/*","this","is","a","
%@ ","multiple","line","
%@ ","block","comment","
%@ ","*/","
%@ ","
%@ ","/*","this","is","an","imbricated","
%@ ","/*","block","comment","
%@ ","*/","
%@ ","*/","
%@ ","
%@ ","
%@ ","/*","this","is","an","imbricated","
%@ ","/*","block","comment","extreme","case","
%@ ","/*","This","should","be","level","3","
%@ ","/*","level","4","
%@ ","/*","level","5","
%@ ","/*","level","6","depth","comment","*/","*/","
%@ ","apples","*/","
%@ ","pears","*/","
%@ ","*/","
%@ ","*/","
%@ ","
%@ ","
%@ ","/*","a","=","a","block","comment","fail","*/","
%@ "]caught: error('$interrupt_thrown',repl)
%@ caught: error('$interrupt_thrown',repl)
%@ 
%@ caught: error('$interrupt_thrown',repl)



/*===========================================================

RegExes for each of the specified lexical elements to construct each
and identify it if needed.

===========================================================*/

digit(D) :- 
    C #>= 48,
    C #=< 57,
    char_code(D,C).

nonzero(NZ) :-
    C #>= 49,
    C #=< 57,
    char_code(NZ,C).

letter(L) :-
    char_code(L,C),
    ((C #>= 65,C #=< 90);
    (C #>= 97,C #=< 122)),!.

space(S) :-
    char_code(S,C),
    C = 32;
    C = 160.

alphanum(C) :-
    (digit(C) ;
     letter(C) ;
    (C = '_')),!.

character(C) :-
    (alphanum(C) ;
    space(C)),!.


% Removed from assignment but present if needed
% string([C]) :-character(C),!.
% string([H|T]) :-
%     character(H),
%     string(T).

integer_([I]):-digit(I),!.
integer_([H|T]) :-
    nonzero(H),
    integer0(T).

integer0([]).
integer0([H|T]) :-
    digit(H),
    integer0(T).

% ?- integer_("8").
%@    true.
% ?- integer_("08").
%@ false.
% ?- integer_("0008").
%@ false.
% ?- integer_("8020").
%@    true.


fraction(['.',D]) :- digit(D),!.
fraction([H|T]) :-
    H = '.',
    fraction0(T).
fraction0([L]):-nonzero(L),!.
fraction0([H|T]) :-
    digit(H),
    fraction0(T).

float_(Fl) :-
    append(I,F,Fl),
    integer_(I),
    fraction(F),!.
float_(Lst) :-
    split_delimiter(Lst,Int,Rest,'.'),
    integer_(Int),
    split_delimiter(Rest,F,E,'e'),
    fraction(F),
    E = ['e'|I],
    integer_(I),!.
float_(Lst) :-
    split_delimiter(Lst,Int,Rest,'.'),
    integer_(Int),
    split_delimiter_m1(Rest,F,E,'-'),
    fraction(F),
    E = ['e','-'|I],
    integer_(I),!.
float_(Lst) :-
    split_delimiter(Lst,Int,Rest,'.'),
    integer_(Int),
    split_delimiter_m1(Rest,F,E,'+'),
    fraction(F),
    E = ['e','+'|I],
    integer_(I),!.

% ?- float_("123.01323e+03").
%@ false.
% ?- float_("123.01323e-833").
%@    true.
% ?- float_("1.01323e-833").
%@    true.
% ?- float_(".01323e-833").
%@ false.
% ?- float_("123.01323e+3").
%@    true.
% ?- float_("123.01323e+30").
%@    true.
% ?- float_("123.01323e+03").
%@ false.
% ?- float_("0123.01323e+3").
%@ false.
% ?- float_("1.23").
%@    true.
% ?- float_("120.34e10").
%@    true.


id([H|T]) :-
    letter(H),
    id0(T).
id0([]).
id0([H|T]) :-
    alphanum(H),
    id0(T).


% ?- id("ddd2s23").
%@    true.


/*===========================================================

List manipulation helper methods

===========================================================*/

% Helper methods for lists
% split_last_n
split_last_n(L,[],L,N) :- length(L,N),!.
split_last_n([H|T1],[H|T0],T,N) :-
    split_last_n(T1,T0,T,N).

% splits at the delimiter minus 1
split_delimiter_m1(L,[],L,D) :- L = [_L1,D|_L2],!.
split_delimiter_m1([H|T1],[H|T0],T,D) :-
    split_delimiter_m1(T1,T0,T,D).

% splits at the delimiter for only the first instance of a result
split_delimiter(L,[],L,D) :- L = [D|_L2],!.
split_delimiter([H|T1],[H|T0],T,D) :-
    split_delimiter(T1,T0,T,D).

% removes the last character of a list
remove_last([_], []):-!.
remove_last([H|L1],[H|L2]):-remove_last(L1,L2).


% removes any of the symbols from the line specified
clean_line([],[]):-!.
clean_line([H1|T1],[H2|T2]):-
    (member(H1,['\t','\r','\n']) ->
	 H2 = ' '; H1 = H2),
    clean_line(T1,T2).


% removes any of the symbols from the line specified
clean_charlist([],[]):-!.
clean_charlist([H1|T1],[H1|T2]):-
    (member(H1,[['\t'],['\r'],['\n'],[' '],'\n',' ']) ->
	 (remove(T1,H1,T2),clean_charlist(T2,T2));
     clean_charlist(T1,T2)).


% removes all occurances of a character from a list
remove([X|Xs],X,Ys) :- remove(Xs,X,Ys).
remove([X|Xs],Z,[X|Ys]) :-
    X \= Z,
    remove(Xs,Z,Ys).
remove([],_X,[]).


% appends 3 lists together
treple(L1,L2,L3,L):-
    append(L12,L3,L),
    append(L1,L2,L12).

%appends 4 lisst together
quadruple(L1,L2,L3,L4,L):-
    append(L123,L4,L),
    append(L12,L3,L123),
    append(L1,L2,L12).

strip_comments([],_Cl,[]).
strip_comments([H|T],Cl0,Out):-
    C10 #> 0,
    token(H,D),
    D = 'Comment start',
    Cl #= Cl0 + 1,
    strip_comments(T,Cl,Out).
strip_comments([H|T],Cl0,Out):-
    C10 #> 0,
    token(H,D),
    D = 'Comment end',
    Cl #= Cl0 - 1,
    strip_comments(T,Cl,Out).
strip_comments([H|T],Cl0,Out):-
    C10 #> 0,
    strip_comments(T,Cl0,Out).
strip_comments([H|T],Cl0,Out):-
    Cl0 #= 0,
    H = "//",
    split_delimiter([H|T],Comment,Rest,"\n"),
    write(Comment),
    write(Rest),
    strip_comments(Rest,Cl0,Out).
strip_comments([H|T],Cl0,Out):-
    Cl0 #= 0,
    token(H,D),
    D = 'Comment start',
    Cl #= Cl0 + 1,
    strip_comments(T,Cl,Out).
strip_comments([H|T1],Cl0,[H|T2]):-
    Cl0 #= 0,
    write(H),
    strip_comments(T1,Cl0,T2).

remove_comment_blocks([H|T]) --> [H], ["/*"], seq(_) ,["*/"], remove_comment_blocks(T).
remove_comment_blocks([H|T]) --> ["/*"], seq(_) ,["*/"],[H], remove_comment_blocks(T).
remove_comment_blocks([H|T]) --> [H], remove_comment_blocks(T).
remove_comment_blocks([]) --> [].


remove_inline([],[]).
remove_inline([H|T],L) :-
    H = "//",
    split_delimiter([H|T],Comment,Rest,['\n']),
    remove_inline(Rest,L),!.
remove_inline([H|T],[H|T2]) :-
    remove_inline(T,T2),!.



/*===========================================================

DCG's written to parse the value into a tokenized list from the
top down with syntax to easily accomodate expanding delimiters.

===========================================================*/


wordlist([X,Z|Y]) --> word(X), whitespace(Z), wordlist(Y).
wordlist([X,Z]) --> whitespace(X), wordlist(Z).
wordlist([X,Z]) --> word(X).
wordlist([X,Z]) --> word(X), whitespace(Z).



% A sequence of characters
seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

% for current readability but likely to change
%word(X) --> X,{token(X,_)}.
word(X) --> seq(X).

% definitions of whitespace or delimiters
whitespace("\n") --> "\n".
% Comments
whitespace("//") --> "//".
whitespace("/*") --> "/*".
whitespace("*/") --> "*/".
% Symbol Operators and brackets
whitespace("->") --> "->".
whitespace(":") --> ":".
whitespace(";") --> ";".
whitespace("[") --> "[".
whitespace("]") --> "]".
whitespace("{") --> "{".
whitespace("}") --> "}".
whitespace("(") --> "(".
whitespace(")") --> ")".
whitespace(".") --> ".".
whitespace(",") --> ",".
% Equality
whitespace("==") --> "==".
whitespace(">=") --> ">=".
whitespace("<=") --> "<=".
whitespace(">") --> ">".
whitespace("<") --> "<".
whitespace("\\=") --> "\\=".
% Arithmatic
whitespace("-") --> "-".
whitespace("+") --> "+".
whitespace("/") --> "/".
whitespace("*") --> "*".
whitespace("=") --> "=".
whitespace("|") --> "|".
whitespace("&") --> "&".
% Designated Whitespace
whitespace(X) -->{X=[]}, " ".
whitespace(X) -->{X=[]}, "\t".
whitespace(X) -->{X=[]}, "\r".
whitespace(X) -->{X=[]}, " ", whitespace(Z).
whitespace(X) -->{X=[]}, "\t", whitespace(Z).
whitespace(X) -->{X=[]}, "\r", whitespace(Z).
 

% a test case
% ?- phrase(wordlist(X),"==\t+\t\n|\t(\t;\tif\n \t\t \t"),!,remove(X,[],Lst).
%@    X = [[],"==",[],[],[],"+",[],[],[],...|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.
%@ caught: error(instantiation_error,char_code/2)
%@    X = ["==",[],"+",[],[],"\n","|",[],"( ...",[]|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.
%@    X = ["==",[],"+",[],[],"\n","|",[],"( ...",[]|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.
%@    X = ["==",[],"+",[],[],"\n","|",[],"( ...",[]|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.


/*

New regex that works with the dcgs

*/

% non zero digits
nonzero('1') --> "1".
nonzero('2') --> "2".
nonzero('3') --> "3".
nonzero('4') --> "4".
nonzero('5') --> "5".
nonzero('6') --> "6".
nonzero('7') --> "7".
nonzero('8') --> "8".
nonzero('9') --> "9".

% digits
digit('0') --> "0".
digit('1') --> "1".
digit('2') --> "2".
digit('3') --> "3".
digit('4') --> "4".
digit('5') --> "5".
digit('6') --> "6".
digit('7') --> "7".
digit('8') --> "8".
digit('9') --> "9".

% Lowercase
letter('a') --> "a".
letter('b') --> "b".
letter('c') --> "c".
letter('d') --> "d".
letter('e') --> "e".
letter('f') --> "f".
letter('g') --> "g".
letter('h') --> "h".
letter('i') --> "i".
letter('j') --> "j".
letter('k') --> "k".
letter('l') --> "l".
letter('m') --> "m".
letter('n') --> "n".
letter('o') --> "o".
letter('p') --> "p".
letter('q') --> "q".
letter('r') --> "r".
letter('s') --> "s".
letter('t') --> "t".
letter('u') --> "u".
letter('v') --> "v".
letter('w') --> "w".
letter('x') --> "x".
letter('y') --> "y".
letter('z') --> "z".

% Uppercase
letter('A') --> "A".
letter('B') --> "B".
letter('C') --> "C".
letter('D') --> "D".
letter('E') --> "E".
letter('F') --> "F".
letter('G') --> "G".
letter('H') --> "H".
letter('I') --> "I".
letter('J') --> "J".
letter('K') --> "K".
letter('L') --> "L".
letter('M') --> "M".
letter('N') --> "N".
letter('O') --> "O".
letter('P') --> "P".
letter('Q') --> "Q".
letter('R') --> "R".
letter('S') --> "S".
letter('T') --> "T".
letter('U') --> "U".
letter('V') --> "V".
letter('W') --> "W".
letter('X') --> "X".
letter('Y') --> "Y".
letter('Z') --> "Z".

alphanum(C) --> letter(C).
alphanum(C) --> digit(C).
alphanum("_") --> ["_"].


character(C) --> alphanum(C).
character(" ") --> [" "].


% ?- phrase(alphanum(C),"1").
%@    C = '1'
%@ ;  false.
%@    C = '1'
%@ ;  false.

% ?- phrase(alphanum(C),["_"]).
%@    C = "_".

seqDigit([])     --> [].
seqDigit([E|Es]) --> digit(E), seqDigit(Es).

integer(X) --> seq(X).

% integer([H]) --> nonzero(H).
% integer([H|T]) --> nonzero(H), seqDigit(T).

% ?- phrase(integer(C),"2909").
%@    C = "2909"
%@ ;  false.
%@    C = "2909"
%@ ;  false.
%@ false.
%@    C = "2".

fraction(L) --> {quadruple(H,".",T,[Ts],L)},integer(H),".",seqDigit(T),nonzero(Ts).


% ?- phrase(fraction(C),"323322.0232232"),!.
%@    C = "323322.0232232".

% temporary measure
%float([Frac,Int]) --> fraction(Frac), "e", fsign, integer(Int).

fsign --> "+".
fsign --> "-".
fsign --> "".

float(X) --> seq(X).

% ?- phrase(float(X),Y).

% ?- phrase(float(C),"323322.0232232e2777").
%@    C = ["323322.0232232","2777"]
%@ ;  
%@ caught: error('$interrupt_thrown',repl)
%@ 
%@ caught: error('$interrupt_thrown',repl)
%@    C = [["323322","023223",'2'],"2777"]
%@ ;  false.


% ?- phrase(float(C),"323322.0232232e27770").
%@    C = [["323322","023223",'2'],"27770"]
%@ ;  false.

% ?- phrase(float(C),"323322.0232232e-2777").
%@    C = [["323322","023223",'2'],"2777"]
%@ ;  false.

% ?- phrase(float(C),"323322.02322320e2777").
%@ false.

id(X) --> seq(X).

%id([I|Rest]) --> letter(I),seqAlphanum(Rest).
%id_inn([I|Rest]) --> letter(I), seqAlphanum(Rest).

% ?- phrase(id(X),[Y]).
%@    Y = "a", X = "a"
%@ ;  Y = "aa", X = "aa"
%@ ;  Y = "aaa", X = "aaa"
%@ ;  Y = "aaaa", X = "aaaa"
%@ ;  Y = "aaaaa", X = "aaaaa"
%@ ;  ...
%@    Y = ["a"], X = "a"
%@ ;  Y = ["a","a"], X = "aa"
%@ ;  Y = ["a","a","a"], X = "aaa"
%@ ;  Y = ["a","a","a","a"], X = "aaaa"
%@ ;  Y = ["a","a","a","a","a"], X = "aaaaa"
%@ ;  Y = ["a","a","a","a","a","a"], X = "aaaaaa"
%@ ;  Y = ["a","a","a","a","a","a","a"], X = "aaaaaaa"

% ?- phrase(id_inn("aa"),X).
%@    X = ["a","a"]
%@ ;  false.

% ?- phrase(seq(Y),["apples"]).
%@    Y = ["apples"]
%@ ;  false.
%@    Y = ["apples"]
%@ ;  false.

id --> letter(I), seqAlphanum(A).

% ?- phrase(id,"apples").
%@    true
%@ ;  false.

seqAlphanum([]) --> [].
seqAlphanum([E|Es]) --> alphanum(E), seqAlphanum(Es).

% ?- phrase(seqAlphanum(X),"apple").
%@    X = "apple"
%@ ;  false.

test --> id.

test2 --> id,id,id.

test3 --> ["func"],id_inn(X),["("],[")"],["->"].

% ?- phrase(test2,[X,Y,X]).
%@    X = a, Y = a
%@ ;  ...

% ?- phrase(test3,["func","i","(",")","->"]).
%@ false.
%@ false.

% ?- phrase(id("Diet_struct"),C).
%@    C = "Diet_struct"
%@ ;  false.


% ?- phrase(id,"Diet_struct").
%@    true
%@ ;  false.

% ?- phrase(id,"id").
%@    true
%@ ;  false.

% ?- length(X,4),phrase(id,X).
%@    X = "a000"
%@ ;  ...

% ?- phrase(test("Apple"),X).
%@ false.
%@ AAppppllee   X = "Apple"
%@ ;  false.


% ?- length(X,2),phrase(test,X).
%@    X = "a0"
%@ ;  X = "a1"
%@ ;  X = "a2"
%@ ;  ...


/* 

Idea for error correction, have a symbol for every symbol that will be proposed missing and print an error if missing when parsing.

*/

/*
------------------------------------------------------------------------------------------

Parser

------------------------------------------------------------------------------------------
*/

/*

test case

error recovery ?
- for every branch block have a final test case thats just a generic sequence and results in printing that there was a semantic error.


*/
/*
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*/

% the program
prog_ast(node(program,X)) --> reptprog0_ast(X).

reptprog0_ast([P|N]) --> structorimplorfunc_ast(P), reptprog0_ast(N).   
reptprog0_ast([]) --> [].

structorimplorfunc_ast(P) --> structdecl_ast(P).
structorimplorfunc_ast(P) --> impldef_ast(P).
structorimplorfunc_ast(P) --> funcdef_ast(P).

% Struct Defs

structdecl_ast(node(struct,[node(id,[ID]),Opt,ReptStruct])) --> {length(ID,1)},
						  ["struct"],id(ID),
						  structinherits_ast(Opt),
					     ["{"], reptstructdecl4_ast(ReptStruct),
					     ["}"], [";"].

reptstructdecl4_ast([node(structdecl,[node(visibility,Vis),Mem])|N]) --> visibility_ast(Vis),
				       memberdecl_ast(Mem),
				       reptstructdecl4_ast(N).   
reptstructdecl4_ast([]) --> [].


reptstructinherits_ast(inherits(id(ID),N)) --> {length(ID,1)},[","], id(ID), reptstructinherits_ast(N).   
reptstructinherits_ast([]) --> [].


structinherits_ast(inherits(id(ID),N)) --> ["inherits"], id(ID), reptstructinherits_ast(N).  
structinherits_ast([]) --> [].

% Impl definition

impldef_ast(impl_def(id(ID),Impl)) --> {length(ID,1)},["impl"], id(ID), ["{"], reptimpldef3_ast(Impl), ["}"].  

reptimpldef3_ast([F|N]) --> funcdef_ast(F), reptimpldef3_ast(N).   
reptimpldef3_ast([]) --> [].

% Function definitions
funcdecl_ast(func_decl(F)) --> funchead_ast(F) ,[";"].  

funcdef_ast(func_def(Head,Body)) --> funchead_ast(Head), funcbody_ast(Body).  

funchead_ast(func_head(id(ID),Params,ReturnType)) --> {length(ID,1)},["func"], id(ID),
			       ["("], fparams_ast(Params), [")"],
			       ["->"], returntype_ast(ReturnType).  

funcbody_ast(func_body(Body)) --> ["{"], reptfuncbody1_ast(Body), ["}"].

reptfuncbody1_ast([func_body(B)|N]) --> vardeclorstat_ast(B), reptfuncbody1_ast(N).
reptfuncbody1_ast([]) --> [].


% any Expression
% often of form of 18*4 or a / (h[3] + 2)
expr_ast(expr(Expr,Rhs)) --> arithexpr_ast(Expr), expr2_ast(Rhs). 

expr2_ast(relop(Op,Arith)) --> relop_ast(Op), arithexpr_ast(Arith).  
expr2_ast([]) --> [].


% arithmetic expressions
arithexpr_ast(arith_expr(Term,Rhs)) --> term_ast(Term), rightrecarithexpr_ast(Rhs).


rightrecarithexpr_ast(addop(Op,Term,N)) --> addop_ast(Op), term_ast(Term), rightrecarithexpr_ast(N).
rightrecarithexpr_ast([]) -->  [].

term_ast(term(Factor,Next)) --> factor_ast(Factor), rightrecterm_ast(Next).


rightrecterm_ast(term(multop(Op),Factor,T)) -> multop_ast(Op), factor_ast(Factor), rightrecterm_ast(T).   
rightrecterm_ast([]) -->  [].


factor_ast(factor(id(I),F2,RVOFC)) --> id(I), factor2_ast(F2), reptvariableorfunctioncall_ast(RVOFC).  
factor_ast(factor(intnum(I))) --> {length(ID,1)},integer(I).  
factor_ast(factor(floatnum(F))) --> {length(ID,1)},float(F).
factor_ast(factor(F)) --> ["("], arithexpr_ast(F), [")"].
factor_ast(factor(not(F))) --> ["!"], factor_ast(F).
factor_ast(factor(sign(S),F)) --> sign_ast(S), factor_ast(F).

factor2_ast(F2) --> ["("], aparams_ast(F2), [")"].
factor2_ast(F2) --> reptidnest1_ast(F2).  

fparams_ast(func_param(id(ID),type(Type),Rept3,Rept4)) --> {length(ID,1)},id(ID), [":"],
							     type_ast(Type),
							     reptfparams3_ast(Rept3),
							     reptfparams4_ast(Rept4).
fparams_ast([]) --> [].

fparamstail_ast(function_parameter_tail(id(ID),type(Type),ReptTail)) --> {length(ID,1)},[","], id(ID), [":"], type_ast(Type),
							 reptfparamstail4_ast(ReptTail).

reptfparams3_ast([Size|Rept3]) --> arraysize_ast(Size), reptfparams3_ast(Rept3).  
reptfparams3_ast([]) --> [].

reptfparams4_ast(repeated_params4(Tail,Param)) --> fparamstail_ast(Tail), reptfparams4_ast(Param).
reptfparams4_ast([]) --> [].

reptfparamstail4_ast([Size|ParamTail]) --> arraysize_ast(Size), reptfparamstail4_ast(ParamTail).  
reptfparamstail4_ast([]) --> [].


arraysize_ast(Size) --> ["["], arraysize2_ast(Size).  

arraysize2_ast(Size) --> integer(Size), ["]"]. 
arraysize2_ast([]) --> ["]"].


reptaparams1_ast(array_parameter(P,N)) --> aparamstail_ast(P), reptaparams1_ast(N).  
reptaparams1_ast([]) --> [].


% array parameters
aparams_ast([array_parameter(P)|T]) --> expr_ast(P), reptaparams1_ast(T).
aparams_ast([]) --> [].

% array paramters tail
aparamstail_ast(X) --> [","], expr_ast(X).  



statement_ast(if(Expr,StatblockIf,StatblockElse)) --> ["if"], ["("], relexpr_ast(Expr), [")"], ["then"],
						      statblock_ast(StatblockIf), ["else"],
						      statblock_ast(StatblockElse), [";"].  
statement_ast(while(Expr,Statblock)) --> ["while"], ["("], relexpr_ast(Expr), [")"],
					 statblock_ast(Statblock), [";"].  
statement_ast(read(Variable)) --> ["read"], ["("], variable_ast(Variable), [")"], [";"].  
statement_ast(write(Expr)) --> ["write"], ["("], expr_ast(Expr), [")"], [";"].  
statement_ast(return(Expr)) --> ["return"], ["("], expr_ast(Expr), [")"], [";"].  
statement_ast(assign(id(ID),Idnest)) --> {length(ID,1)},id(ID), statementidnest_ast(Idnest), [";"]. 

statementidnest_ast(dot(id(ID),Idnest)) --> {length(ID,1)}, ["."], id(ID), statementidnest_ast(Idnest).
statementidnest_ast(parens(Params,Idnest)) --> ["("], aparams_ast(Params), [")"], statementidnest2_ast(Idnest).
statementidnest_ast(indice(Indice,Idnest,StatementIdNest)) --> indice_ast(Indice), reptidnest1_ast(Idnest), statementidnest3(StatementIdNest).
statementidnest_ast(assign(Expr)) --> assignop_ast, expr_ast(Expr).


statementidnest2_ast(nested_id(id(ID),Nest)) --> {length(ID,1)}, ["."], id(ID), statementidnest_ast(Nest).
statementidnest2_ast([]) --> [].

statementidnest3(assign(Expr)) --> assignop_ast, expr_ast(Expr).
statementidnest3(dot(id(ID),IDnest)) --> ["."], id(ID), statementidnest_ast(IDnest).  

reptidnest1_ast(idnest(Indice,N)) --> indice_ast(Indice), reptidnest1_ast(N).  
reptidnest1_ast([]) --> [].

reptvariableorfunctioncall_ast(id_nest(id(ID),N)) -->  idnest_ast(ID), reptvariableorfunctioncall_ast(N).  
reptvariableorfunctioncall_ast([]) -->  [].

idnest_ast(dot(idnest(id(ID),N))) --> ["."], id(ID), idnest2_ast(N).  
idnest2_ast(param(Param)) --> ["("], aparams_ast(Param), [")"].  
idnest2_ast(N) --> reptidnest1_ast(N).  

indice_ast(indice(Indice)) --> ["["], arithexpr_ast(Indice), ["]"].  

memberdecl_ast(member(Fun)) --> funcdecl_ast(Fun).  
memberdecl_ast(Var) --> vardecl_ast(Var).  

relexpr_ast(relational_expr(Arith,Rel)) --> arithexpr_ast(Arith),
						    rightrelexpr(Rhs).
rightrelexpr(relop(Rel,Arith)) --> relop_ast(Rel),
				   arithexpr_ast(Arith).

reptstatblock1_ast(statblock(S,N)) --> statement_ast(S), reptstatblock1_ast(N).   
reptstatblock1_ast([]) --> [].


reptvardecl4_ast(var(Size,N)) --> arraysize_ast(Size), reptvardecl4_ast(N).   
reptvardecl4_ast([]) --> [].

		 
statblock_ast(statblock(S)) --> ["{"], reptstatblock1_ast(S), ["}"].   
statblock_ast(statblock(S)) --> statement_ast(S).   
statblock_ast([]) -->  [].

vardecl_ast(var(id(ID),Type,N)) --> ["let"], id(ID), [":"], type_ast(Type), reptvardecl4_ast(N), [";"].   

vardeclorstat_ast(V) --> vardecl_ast(V).   
vardeclorstat_ast(S) --> statement_ast(S).   

variable_ast(var(id(ID),N)) -->  id(ID), variable2_ast(N).  

variable2_ast(var(Idnest,N)) -->  reptidnest1_ast(Idnest), reptvariable_ast(N).    
variable2_ast(var(Param,Idnest)) -->  ["("], aparams_ast(Param), [")"], varidnest_ast(Idnest).

reptvariable_ast(repeated_var(V,N)) -->  varidnest_ast(V), reptvariable_ast(N).   
reptvariable_ast([]) --> [].

varidnest_ast(var(id(ID),N)) --> ["."], id(ID), varidnest2_ast(N).
varidnest2_ast(nest(P,Idnest)) --> ["("], aparams_ast(P), [")"], varidnest(Idnest).  
varidnest2_ast(Idnest) --> reptidnest1_ast(Idnest).


relop_ast("=") --> ["="].
relop_ast("!=") -->  ["!="].
relop_ast("<") --> ["<"].
relop_ast(">") --> [">"].
relop_ast("<=") -->  ["<="].
relop_ast(">=") -->  [">="].

		 
returntype_ast(type(T)) --> type_ast(T).   
returntype_ast(type("void")) --> ["void"].

sign_ast("+") --> ["+"].
sign_ast("-") --> ["-"].

assignop_ast --> ["="].
		       
type_ast("integer") --> ["integer"].
type_ast("float") --> ["float"].
type_ast("id") --> ["id"].
type_ast(ID) --> id(ID).



% addition operators 
addop_ast("+") --> ["+"].
addop_ast("-") --> ["-"].
addop_ast("|") --> ["|"].

% multiplication operators
multop_ast("*") --> ["*"].
multop_ast("/") --> ["/"].
multop_ast("&") --> ["&"].

visibility_ast("public") --> ["public"].  
visibility_ast("private") -->["private"].


/*======================================================================

Abstract Syntax Tree Generator

======================================================================*/


ast_generation(Filein,Fileout) :-
    open(Filein,read,S1,[]),
    open(Fileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    phrase(remove_comment_blocks(Lst3),Lst2),
    remove_inline(Lst3,Lst4),
    remove(Lst4,"\n",Lst),
%    write(Lst),
    phrase(prog_ast(AST),Lst),!,
    write(S2,AST),
    close(S1),
    close(S2).

/*======================================================================

Main Program Tests

======================================================================*/

% ?- ast_generation("bubblesort.src","bubblesort_ugly_ast.txt").
%@    true.


% ?- ast_generation("polynomial.src","polynomial_ugly_ast.txt").
%@    true.

/*======================================================================

A whole bunch of tests with their results to show more output

======================================================================*/


% ?- phrase(prog_ast(X),["func",Y,"(",")","->","void","{","}","struct","POLYNOMIAL","{","public","func","evaluate","(","x",":","float",")","->","float",";","}",";"]).



% ?- phrase(prog_ast(X),["struct","POLYNOMIAL","{","public","func","evaluate","(","x",":","float",")","->","float",";","}",";"]).
%@    X = program([struct(id(["POLYNOMIAL"]),[],[struct_declaration(visibility("public"),member(func_decl(func_head(id(["evalua ..."]),func_param(id(["x"]),type("float"),[],[]),type("float")))))])|_A])
%@ ;  false.


% ?- open("testoutugly.txt",write,S,[]),phrase(prog_ast(X),["struct","POLYNOMIAL","{","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","struct","LINEAR","inherits","POLYNOMIAL","{","private","let","a",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","private","let","b",":","float",";","public","func","build","(","A",":","float,","B",":","float",")","->","LINEAR",";","public","func","evaluate","(","x",":","float",")","->","float",";","}",";"]),!,write(S,X),close(S).
%@ false.
%@    S = '$stream'(0x5619d4b36450), X = program([struct(id(["POLYNOMIAL"]),[],[struct_declaration(visibility("public"),member(func_decl(func_head(id(["evalua ...","(","x",":",...]),func_param(id(["x"]),type("float"),[],[]),type("float")))))])|_A]).

% ?- phrase(statement_ast(X),["write","(","arr","[","i","]",")",";"]),!.
%@    X = write(expr(arith_expr(term(factor(id(["arr"]),idnest(indice(arith_expr(term(factor(id(["i"]),[],[]),[]),[])),[]),[]),[]),[]),[])).


%?- phrase(statement_ast(X),["id","(","id",")",".","id","=","1",";"]),!.
%@    X = assign(id(["id"]),parens([array_parameter(expr(arith_expr(term(factor(id(["id"]),[],[]),[]),[]),[]))],nested_id(id(["id"]),assign(expr(arith_expr(term(factor(id(["1"]),[],[]),[]),[]),[]))))).
/* The previous query result expanded by hand
    X = 
assign(id(["id"]),
  parens([array_parameter(
    expr(
      arith_expr(
        term(
          factor(
            id(["id"]),
          [],[]),
         [])
      ,[]),
    [])
   )],
   nested_id(
     id(["id"]),
     assign(
     expr(
       arith_expr(
         term(
           factor(
             id(["1"]),[],[]),[]),[]),[])))))
*/


%?- phrase(statement_ast(X),["id","(","id",")",".","id","(",")","=","1",";"]),!.
%@ false.
%@    X = assign(id(["id"]),parens([array_parameter(expr(arith_expr(term(factor(id(["id"]),[],[]),[]),[]),[]))],nested_id(id(["id"]),parens([array_parameter(expr(arith_expr(term(factor(id([]),[],[]),[]),[]),[]))],assign(expr(arith_expr(term(factor(id(["1"]),[],[]),[]),[]),[])))))).

%"id",".","id","[","id","]","=","1",";","id","[","id","]",".","id","=","1",";","id","[","id","]",".","id","[","id","]","=","1",";","id",".","id","[","id","]","[","id","]","=","1",";","id","[","id","]","[","id","]",".","id","=","1",";","id","[","id","]","[","id","]",".","id","[","id","]","[","id","]","=","1",";","id","(","id",")",".","id","[","id","]","=","1",";","id","(","id",")",".","id","[","id","]","[","id","]","=","1",";","}"]
%@ caught: error('$interrupt_thrown',repl)




% ?- phrase(prog_ast(X),["func","bubbleSort","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","{","let","n",":","integer",";","let","i",":","integer",";","let","j",":","integer",";","let","temp",":","integer",";","n","=","size",";","i","=","0",";","j","=","0",";","temp","=","0",";","while","(","i","<","n","-","1",")","{","while","(","j","<","n","-","i","-","1",")","{","if","(","arr","[","j","]",">","arr","[","j","+","1","]",")","then","{","temp","=","arr","[","j","]",";","arr","[","j","]","=","arr","[","j","+","1","]",";","arr","[","j","+","1","]","=","temp",";","}","else",";","j","=","j","+","1",";","}",";","i","=","i","+","1",";","}",";","}","func","printArray","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","{","let","n",":","integer",";","let","i",":","integer",";","n","=","size",";","i","=","0",";","while","(","i","<","n",")","{","write","(","arr","[","i","]",")",";","i","=","i","+","1",";","}",";","}",";"]).
%@ caught: error(existence_error(procedure,phrase/2),phrase/2)
%@ 
%@ caught: error('$interrupt_thrown',repl)


% ?- phrase(statement_ast(X),["while","(","i","<","n","-","1",")","{","while","(","j","<","n","-","i","-","1",")","{","if","(","arr","[","j","]",">","arr","[","j","+","1","]",")","then","{","temp","=","arr","[","j","]",";","arr","[","j","]","=","arr","[","j","+","1","]",";","arr","[","j","+","1","]","=","temp",";","}","else",";","j","=","j","+","1",";","}",";","i","=","i","+","1",";","}",";"]),!.
%@    X = while(relational_expr(arith_expr(term(factor(id(["i"]),[],[]),[]),[]),_A),statblock(statblock(while(relational_expr(arith_expr(term(factor(id(["j"]),[],[]),[]),[]),_B),statblock(statblock(if(relational_expr(arith_expr(term(factor(id(["arr"]),idnest(indice(arith_expr(term(factor(id(["j"]),[],[]),[]),[])),[]),[]),[]),[]),_C),statblock(statblock(assign(id(["temp"]),assign(expr(arith_expr(term(factor(id(["a ..."]),idnest(indice(arith_expr(term(factor(id(...),[],[]),[]),[])),[]),[]),[]),[]),[]))),statblock(assign(id(["arr"]),indice(indice(arith_expr(term(factor(id(["j"]),[],[]),[]),[])),[],assign(expr(arith_expr(term(factor(id([...]),idnest(indice(arith_expr(term(...),addop(...))),[]),[]),[]),[]),[])))),statblock(assign(id(["arr"]),indice(indice(arith_expr(term(factor(id([...]),[],[]),[]),addop("+",term(factor(id([...]),[],[]),[]),[]))),[],assign(expr(arith_expr(term(factor(id([...]),[],[]),[]),[]),[])))),[])))),[]),statblock(assign(id(["j"]),assign(expr(arith_expr(term(factor(id(["j"]),[],[]),[]),addop("+",term(factor(id(["1"]),[],[]),[]),[])),[]))),[])))),statblock(assign(id(["i"]),assign(expr(arith_expr(term(factor(id(["i"]),[],[]),[]),addop("+",term(factor(id(["1"]),[],[]),[]),[])),[]))),[])))).
%@    X = while(relational_expr(arith_expr(term(factor(id(["i"]),[],[]),[]),[]),_A),statblock(statblock(while(relational_expr(arith_expr(term(factor(id(["j"]),[],[]),[]),[]),_B),statblock(statblock(if(relational_expr(arith_expr(term(factor(id(["arr"]),idnest(indice(arith_expr(term(factor(id(["j"]),[],[]),[]),[])),[]),[]),[]),[]),_C),statblock(statblock(assign(id(["temp"]),assign(expr(arith_expr(term(factor(id(["a ..."]),idnest(indice(arith_expr(term(factor(id(...),[],[]),[]),[])),[]),[]),[]),[]),[]))),statblock(assign(id(["arr"]),indice(indice(arith_expr(term(factor(id(["j"]),[],[]),[]),[])),[],assign(expr(arith_expr(term(factor(id([...]),idnest(indice(arith_expr(term(...),addop(...))),[]),[]),[]),[]),[])))),statblock(assign(id(["arr"]),indice(indice(arith_expr(term(factor(id([...]),[],[]),[]),addop("+",term(factor(id([...]),[],[]),[]),[]))),[],assign(expr(arith_expr(term(factor(id([...]),[],[]),[]),[]),[])))),[])))),[]),statblock(assign(id(["j"]),assign(expr(arith_expr(term(factor(id(["j"]),[],[]),[]),addop("+",term(factor(id(["1"]),[],[]),[]),[])),[]))),[])))),statblock(assign(id(["i"]),assign(expr(arith_expr(term(factor(id(["i"]),[],[]),[]),addop("+",term(factor(id(["1"]),[],[]),[]),[])),[]))),[])))).


% ?- phrase(statement_ast(X),["while","(","i","<","n","-","1",")","{","i","=","i","+","1",";","}",";"]),!.
%@    X = while(relational_expr(arith_expr(term(factor(id(["i"]),[],[]),[]),[]),_A),statblock(statblock(assign(id(["i"]),assign(expr(arith_expr(term(factor(id(["i"]),[],[]),[]),addop("+",term(factor(id(["1"]),[],[]),[]),[])),[]))),[]))).


% ?- phrase(statement_ast(X),["while","(",")","{","}",";"]).
%@ false.

% ?- phrase(prog_ast(X),["struct","POLYNOMIAL","{","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","struct","LINEAR","inherits","POLYNOMIAL","{","private","let","a",":","float",";","private","let","b",":","float",";","public","func","build","(","A",":","float,","B",":","float",")","->","LINEAR",";","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","struct","QUADRATIC","inherits","POLYNOMIAL","{","private","let","a",":","float",";","private","let","b",":","float",";","private","let","c",":","float",";","public","func","build","(","A",":","float,","B",":","float,","C",":","float",")","->","QUADRATIC",";","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","impl","POLYNOMIAL","{","func","evaluate","(","x",":","float",")","->","float","{","return","(","0",")",";","}","}","impl","QUADRATIC","{","func","evaluate","(","x",":","float",")","->","float","{","let","result",":","float",";","result","=","a",";","result","=","result","*","x","+","b",";","result","=","result","*","x","+","c",";","return","(","result",")",";","}","func","build","(","A",":","float,","B",":","float,","C",":","float",")","->","QUADRATIC","{","let","new_function",":","QUADRATIC",";","new_function",".","a","=","A",";","new_function",".","b","=","B",";","new_function",".","c","=","C",";","return","(","new_function",")",";","}","}"]),!.
%@    X = program([struct(id(["POLYNOMIAL"]),[],[struct_declaration(visibility("public"),member(func_decl(func_head(id(["evalua ..."]),func_param(id(["x"]),type("float"),[],[]),type("float")))))]),struct(id(["LINEAR"]),inherits(id(["POLYNOMIA ..."]),[]),[struct_declaration(visibility("private"),var(var(id(["a"]),"float",[]))),struct_declaration(visibility("private ..."),var(var(id(["b"]),"float",[]))),struct_declaration(visibility("publi ..."),member(func_decl(func_head(id(["build"]),func_param(id([...]),type([...|...]),[],[]),type(["LINEAR"]))))),struct_declaration(visibility("pub ..."),member(func_decl(func_head(id([...]),func_param(id(...),type(...),[],[]),type("float")))))]),struct(id(["QUADRATI ..."]),inherits(id(["POLYNOM ..."]),[]),[struct_declaration(visibility("private ..."),var(var(id(["a"]),"float",[]))),struct_declaration(visibility("priva ..."),var(var(id(["b ..."]),"floa ...",[]))),struct_declaration(visibility("pri ..."),var(var(id([...]),"fl ...",[]))),struct_declaration(visibility("p ..."),member(func_decl(func_head(id(...),func_param(...),type(...))))),struct_declaration(visibility(...),member(func_decl(...)))]),impl_def(id(["POLYNO ..."]),[func_def(func_head(id(["ev ..."]),func_param(id(["x ..."]),type("flo ..."),[],[]),type("floa ...")),func_body([func_body(return(expr(arith_expr(term(...),[]),[])))]))]),impl_def(id(["QUAD ..."]),[func_def(func_head(id(["evaluate"]),func_param(id([...]),type("f ..."),[],[]),type("fl ...")),func_body([func_body(var(id(...),...,[])),func_body(...)|...])),func_def(func_head(id([...]),func_param(id(...),type(...),[],[]),type([...])),func_body([func_body(...)|...]))])]).


% ?- phrase(prog_ast(X),["impl","QUADRATIC","{","func","evaluate","(","x",":","float",")","->","float","{","let","result",":","float",";","result","=","a",";","result","=","result","*","x","+","b",";","result","=","result","*","x","+","c",";","return","(","result",")",";","}","func","build","(","A",":","float,","B",":","float,","C",":","float",")","->","QUADRATIC","{","let","new_function",":","QUADRATIC",";","new_function",".","a","=","A",";","new_function",".","b","=","B",";","new_function",".","c","=","C",";","return","(","new_function",")",";","}","}"]),!.
%@    X = program([impl_def(id(["QUADRATIC"]),[func_def(func_head(id(["evaluate ..."]),func_param(id(["x"]),type("float"),[],[]),type("float")),func_body([func_body(var(id(["resu ..."]),"float",[])),func_body(assign(id(["re ..."]),assign(expr(arith_expr(term(factor(id(...),[],[]),[]),[]),[])))),func_body(assign(id(["result"]),assign(expr(arith_expr(term(factor(...),[]),addop(...,term(...),[])),[])))),func_body(assign(id([...]),assign(expr(arith_expr(...),[])))),func_body(return(expr(...)))])),func_def(func_head(id(["build"]),func_param(id(["A"]),type(["float ...","B",": ...",...|...]),[],[]),type(["QUADRA ..."])),func_body([func_body(var(id(["ne ..."]),["QUA ..."],[])),func_body(assign(id(["new_function"]),dot(id([...]),assign(expr(arith_expr(term(...),[]),[]))))),func_body(assign(id([...]),dot(id(...),assign(expr(...))))),func_body(assign(id(...),dot(...))),func_body(...)]))])]).

% ?- open("testoutugly.txt",write,S,[]),phrase(prog_ast(X),["func","evaluate","(","x",":","float",")","->","void","{","let","result",":","float",";","result","=","a",";","result","=","result","*","x","+","b",";","result","=","result","*","x","+","c",";","return","(","result",")",";","}","func","evaluate","(","x",":","float",")","->","void","{","let","result",":","float",";","result","=","a",";","result","=","result","*","x","+","b",";","result","=","result","*","x","+","c",";","return","(","result",")",";","}"]),!, write(S,X),close(S).
%@    S = '$stream'(0x5631eb82e930), X = program([func_def(func_head(id(["evaluate"]),func_param(id(["x"]),type("float"),[],[]),type(["void"])),func_body([func_body(var(id(["result"]),"float",[])),func_body(assign(id(["resul ..."]),assign(expr(arith_expr(term(factor(id(["a"]),[],[]),[]),[]),[])))),func_body(assign(id(["res ..."]),assign(expr(arith_expr(term(factor(id([...]),[],[]),[]),addop("+",term(factor(id(...),[],[]),[]),[])),[])))),func_body(assign(id(["r ..."]),assign(expr(arith_expr(term(factor(id(...),[],[]),[]),addop("+",term(factor(...),[]),[])),[])))),func_body(return(expr(arith_expr(term(factor(...),[]),[]),[])))])),func_def(func_head(id(["evaluate"]),func_param(id(["x"]),type("float"),[],[]),type(["void"])),func_body([func_body(var(id(["resul ..."]),"float",[])),func_body(assign(id(["res ..."]),assign(expr(arith_expr(term(factor(id([...]),[],[]),[]),[]),[])))),func_body(assign(id(["r ..."]),assign(expr(arith_expr(term(factor(id(...),[],[]),[]),addop("+",term(factor(...),[]),[])),[])))),func_body(assign(id([...]),assign(expr(arith_expr(term(...),addop(...)),[])))),func_body(return(expr(arith_expr(...),[])))]))]).


/*
%@    X = 
program(
  [function_def(
    function_head(
      id(["evaluate"]),
      function_parameter(
        id(["x"]),
        "float",[],[]),
        type("float")),
    function_body(
      [function_body(
        var(
          id(["result"]),
	  "float",[])),
      function_body(
        statememt_id(
	  id(["resul ..."]),
	  assign(
	    expr(
	      arith_expr(
	        term(
		  term(
		    factor(
		      id([...]),[],[]),
		    [])),
		  []),
		[])))),
      function_body(
        statememt_id(
	  id(["res ..."]),
	  assign(
	    expr(
	      arith_expr(
		term(
		  term(
		    factor(
		      id(...),[],[]),
		  [])),
		  addop("+",
		    term(
		      factor(
		        id(...),[],[]),
		    []),
		[])),
      [])))),
      function_body(
      statememt_id(
        id(["r ..."]),
        assign(
          expr(
            arith_expr(
              term(
                term(
                  factor(...),[])),addop("+",term(factor(...),[]),[])),[])))),function_body(statememt_id(id([...]),statement_id([array_parameter(...)],[])))]))])
%@ ;  ...
*/

/*
[funchead("func",id("build"),
  "(","A",":","float",",","B",":","float",",","C",":","float",")",
    "->","QUADRATIC",
    "{",
      "let","new_function",":","QUADRATIC",";",
      "new_function",".","a","=","A",";",
      "new_function",".","b","=","B",";",
      "new_function",".","c","=","C",";",
      "return","(","new_function",")",";",
"}"]


"func","build","(","A",":","float",",","B",":","float",")","->","LINEAR",";","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","struct","QUADRATIC","inherits","POLYNOMIAL","{","private","let","a",":","float",";","private","let","b",":","float",";","private","let","c",":","float",";"
*/


% ?- phrase(prog_ast(X),["func","build","(","A",":","float",",","B",":","float",",","C",":","float",")","->","QUADRATIC","{","let","new_function",":","QUADRATIC",";","new_function",".","a","=","A",";","new_function",".","b","=","B",";","new_function",".","c","=","C",";","return","(","new_function",")",";","}"]),!.
%@    X = program([func_def(func_head(id(["build"]),func_param(id(["A"]),type("float"),[],repeated_params4(function_parameter_tail(id(["B"]),type("float"),[]),repeated_params4(function_parameter_tail(id(["C"]),type("float"),[]),[]))),type(["QUADRATIC"])),func_body([func_body(var(id(["new_fun ..."]),["QUADRATI ..."],[])),func_body(assign(id(["new_f ..."]),dot(id(["a"]),assign(expr(arith_expr(term(factor(id([...]),[],[]),[]),[]),[]))))),func_body(assign(id(["new ..."]),dot(id(["b"]),assign(expr(arith_expr(term(factor(id(...),[],[]),[]),[]),[]))))),func_body(assign(id(["n ..."]),dot(id(["c"]),assign(expr(arith_expr(term(factor(...),[]),[]),[]))))),func_body(return(expr(arith_expr(term(factor(...),[]),[]),[])))]))]).

% ?- phrase(prog_ast(X),["func","build","(","A",":","float,","B",":","float,","C",":","float",")","->","QUADRATIC","{","let","new_function",":","QUADRATIC",";","new_function",".","a","=","A",";","new_function",".","b","=","B",";","new_function",".","c","=","C",";","return","(","new_function",")",";","}"]),!.
%@    X = program([func_def(func_head(id(["build"]),func_param(id(["A"]),type(["float,","B",":","floa ...","C",":",...]),[],[]),type(["QUADRATIC"])),func_body([func_body(var(id(["new_fun ..."]),["QUADRATI ..."],[])),func_body(assign(id(["new_f ..."]),dot(id(["a"]),assign(expr(arith_expr(term(factor(id([...]),[],[]),[]),[]),[]))))),func_body(assign(id(["new ..."]),dot(id(["b"]),assign(expr(arith_expr(term(factor(id(...),[],[]),[]),[]),[]))))),func_body(assign(id(["n ..."]),dot(id(["c"]),assign(expr(arith_expr(term(factor(...),[]),[]),[]))))),func_body(return(expr(arith_expr(term(factor(...),[]),[]),[])))]))]).


/*

["func","build","(","A",":","float,","B",":","float,","C",":","float",")","->","QUADRATIC",
"{",
  "let","new_function",":","QUADRATIC",";",
  "new_function",".","a","=","A",";",
  "new_function",".","b","=","B",";",
  "new_function",".","c","=","C",";",
  "return","(","new_function",")",";",
"}"]

*/
