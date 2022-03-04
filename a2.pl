/*===========================================================
Author: Alexander De Laurentiis

===========================================================*/

:-use_module(library(files)).
:-use_module(library(charsio)).
:-use_module(library(clpz)).
:-use_module(library(dcgs)).
:-use_module(library(lists)).


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

remove_comments([],_Cl,[]).
remove_comments([H|T],Cl0,Out):-
    C10 #> 0,
    token(H,D),
    D = 'Comment start',
    Cl #= Cl0 + 1,
    remove_comments(T1,Cl,Out).
remove_comments([H|T],Cl0,Out):-
    C10 #> 0,
    token(H,D),
    (D = 'Comment end'
    -> Cl #= Cl0 - 1;
     Cl = Cl0),
    Cl #= Cl0 + 1,
    remove_comments(T1,Cl,Out).
remove_comments([H1|T1],Cl0,Out):-
    Cl0 #= 0,
    H = "//",
    split_delimiter([H|T],_Comment,Rest,['\n']),
    remove_comments(Rest,Cl0,Out).
remove_comments([H|T1],Cl0,[H|T2]):-
    Cl0 #= 0,
    remove_comments(T1,Cl0,T2).


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



/*===========================================================

DCG's written to parse the value into a tokenized list from the
top down with syntax to easily accomodate expanding delimiters.

===========================================================*/


wordlist([X,Z|Y]) --> word(X), whitespace(Z), wordlist(Y).
wordlist([X,Z]) --> whitespace(Z), wordlist(X).
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

alphanum(C) --> digit(C).
alphanum(C) --> letter(C).
alphanum('_') --> "_".


character(C) --> alphanum(C).
character(' ') --> " ".


% ?- phrase(alphanum(C),"1").
%@    C = '1'
%@ ;  false.
%@    C = '1'
%@ ;  false.


% ?- phrase(alphanum(C),"_").
%@    C = '_'.
%@    C = '_'.


seqDigit([])     --> [].
seqDigit([E|Es]) --> digit(E), seqDigit(Es).


integer([H]) --> nonzero(H).
integer([H|T]) --> nonzero(H), seqDigit(T).

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

float([Frac,Int]) --> fraction(Frac), "e", fsign, integer(Int).

fsign --> "+".
fsign --> "-".
fsign --> "".


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

id([I|Rest]) --> letter(I),seqAlphanum(Rest).

% ?- phrase(id("apples"),Y).
%@ false.
%@    Y = "apples"
%@ ;  false.
%@ false.
%@    Y = "apples"
%@ ;  false.

% ?- phrase(id(Y),["apples"]).
%@ false.
%@ false.

id --> letter(I),seqAlphanum(D).


seqAlphanum([]) --> [].
seqAlphanum([E|Es]) --> alphanum(E), seqAlphanum(Es).


test --> id.

test2 --> id,id,id.

test3 --> ["func"],id(X),["("],[")"],["->"].

% ?- phrase(test2,[X,Y,X]).
%@    X = a, Y = a
%@ ;  ...


% ?- phrase(test3,["func","i","(",")","->"]).
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
start([start|T]) --> program(T).

program([program|T]) --> structOrImplOrFuncplus(T).

structOrImplOrFuncplus([H|T]) --> structOrImplOrFunc(H), {write(d1),nl},structOrImplOrFuncplus(T)
			   |[].

structOrImplOrFunc(P) --> structDecl(P)
		       | implDef(P)
		       | funcDef(P).

structDecl([structDecl,H2|[T1|T2]]) --> ["struct"], seq(ID), optstructdecl2(T1), extraId(T2),
	       ["{"],
	       visibilityMemberDef(H2),
	       ["}"],[";"].

optstructdecl2([optionalStructDecl]) --> ["inherits"],id,[','],id.
optstructdecl2([]) -->[].

implDef([implDef|T]) --> ["impl"],["id"],
	       ["{"],
	       funcDefPlus(T),
	       ["}"].


funcDefPlus([H|T]) --> funcDef(H),{write(d2),nl},funcDefPlus(T).
funcDefPlus([]) --> [].

funcDef([funcDef|[P1|P2]]) --> funcHead(P1), funcBody(P2).

extraId([extraID|T]) --> id,[","],extraId(T).
extraId([]) -->[].

visibilityMemberDecl([H1,H2|T]) --> visibility(H1), memberDecl(H2),visibilityMemberDecl(T).
visibilityMemberDecl([]) --> [].

visibility([public]) --> ["public"].
visibility([private]) --> ["private"].

memberDecl(H) --> funcDecl(H).
memberDecl(H) --> varDecl(H).

funcDecl([funcDecl|T]) --> funcHead(T), [";"].

funcHead((funcHead,ID,Params,Return)) --> {write(d3),nl},["func"], seq(ID),
					      ["("],fParams(Params),[")"],
					      ["->"],returnType(Return).

funcBody([funcBody|T]) --> ["{"], varDeclOrStatPlus(T) , ["}"].

varDeclOrStatPlus([H|T]) --> varDeclOrStat(H), varDeclOrStatPlus(T).
varDeclOrStatPlus([]) -->[].

varDeclOrStat(L) --> varDecl(L)
		  |statement(L).

varDecl([varDecl|T]) --> ["let"], seq(X), [":"],type,arraySizePlus,[";"].

arraySizePlus([H|T]) --> arraySize(H), arraySizePlus(T).
arraySizePlus([]) -->[].

statement(funcall) --> idnest, statement2, [";"].
statement(if) -->["if"], ["("], relExpr, [")"],
		 ["then"], statBlock,
		 ["else"], statBlock, [";"].
statement(while) -->["while"], ["("],relExpr, [")"],
		 statBlock,[";"].
statement(read) -->["read"], ["("], variable, [")"], [";"].
statement(write) -->["write"], ["("], expr, [")"], [";"].
statement(return) -->["return"], ["("], expr, [")"], [";"].

statement2 --> id, indice, assignop, expr
	       | ["("], aparams, [")"].

%assignStat --> variable, assignOp, expr.

statBlock --> ["{"], statementPlus, ["}"]
	      |statement
	      |[].

statementPlus --> statement, statementPlus
		  | [].

expr --> arithExpr, expr2.

expr2 --> relOp, arithExpr
	  |[].

relExpr --> arithExpr, relOp, arithExpr.


arithExpr --> term, rightRightRecArithExpr.

rightRightRecArithExpr --> addop, term, rightRightRecArithExpr
			   |[].

sign --> ["+"].
sign --> ["-"].

term --> factor, rightRecTerm.

rightRecTerm --> multop, term, rightRecTerm
		 |[].

factor --> idnest, fidnest
	   |["intLit"]
	   |["floatLit"]
	   |["("], arithExpr, [")"]
	   |["!"], factor
	   |sign, factor.

fidnest --> id, fid1.

fid1 --> indice
	 |["("], aparams, [")"].

variable --> idnestPlus, ["id"], indicePlus.

functionCall --> idnestPlus, ["id"], ["("], aParams, [")"].

idnestPlus --> idnest, idnestPlus
	       |[].

indicePlus -->  indice, indicePlus
		|[].
indice --> ["["], arithExpr, ["]"].

idnest -->  id, fid, ["."]
	   |["("], aparams, [")"],["."].

arraySize(X) --> ["["], arraySizeP(X), ["]"].

arraySizeP(X) --> integer(X).
arraySizeP([]) --> [].


type("integer") --> ["integer"].
type("float") --> ["float"].
type("id") --> ["id"].


returnType(T) --> type(T).
returnType("void") --> ["void"].

fParams([fparams,T,A,F]) --> {write(I)},id, [":"], type(T), arraySizePlus(A), fParamsTailPlus(S,F).
fParams([]) --> [].


arraySizePlus(S,[H|T]) --> arraySize(H), arraySizePlus(S,T)
		  |[].

fParamsTailPlus(S,[H|T]) -->  fParamsTail, fParamsTailPlus(S,T).
fParamsTailPlus(S,[]) --> [].

fParamsTail -->{write("fParamsTail")}, [","], id, [":"], type, arraySizePlus.

aParams([aparams|T]) --> expr, aParamsTailPlus(T).
aParams([]) --> [].

aParamsTailPlus --> aParamsTail, aParamsTailPlus
		    |[].

aParamsTail --> [","], expr.

assignOp --> ["="].

relOp --> ["eq"].
relOp --> ["neq"].
relOp --> ["lt"].
relOp --> ["gt"].
relOp --> ["leq"].
relOp --> ["geq"].

addOp --> ["+"].
addOp --> ["-"].
addOp --> ["or"].

multOp --> ["*"].
multOp --> ["/"].
multOp --> ["and"].
*/


/*

test case

error recovery idea
- for every branch block have a final test case thats just a generic sequence and results in printing that there was a semantic error.

*/


test_file(Filein) :-
    open(Filein,read,S1,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    remove_comments(Ls2,0,Lst),
    write(Lst),nl,
    remove(Lst,"\n",In),
    write(In),
    phrase(start(O),In),
    write(O),
    close(S1).



% make sure to skip newline and special characters
% clean the input of those and comments first

% ?- test_file("test.src").
%@ []
%@ []d3
%@ [start,program,_931|_932]   true
%@ ;  caught: error(instantiation_error,char_code/2)
%@ []
%@ []d3
%@ [start,program,_931|_932]   true
%@ ;  caught: error(instantiation_error,char_code/2)
%@ ["func","id","(",")","->","void","
%@ ","{","
%@ ","
%@ ","}","
%@ "]
%@ ["s","id","(",")","->","void","{","}"]"fParams""void"   true
%@ ;  false.

% ?- test_file("polynomial.src").
%@ []
%@ []d3
%@ [start,program,_52355|_52356]   true

%["func","i","(",")","->","void","{","}","func","as","(",")","->","void","{","}"]

% ?- phrase(start(Se),["struct","i","(","id",":","id",")","->","void","{","}","func","as","(",")","->","void","{","}"]), write(Se).
%@ caught: error(existence_error(procedure,visibilityMemberDef/3),visibilityMemberDef/3)
%@ d3
%@ _746_801d1
%@ d3
%@ [start,program,[funcDef,(funcHead,["i","(","id",":","id",")","->","void","{","}","func","as"],[],"void"),funcBody],_814|_815]   Se = [start,program,[funcDef,(funcHead,["i","(","i ...",...|...],[],"void"),funcBody],_A|_B]
%@ ;  false.
%@ d3
%@ false.
%@ caught: error(existence_error(procedure,visibilityMemberDef/3),visibilityMemberDef/3)
%@ d3
%@ _746_801d1
%@ d3
%@ [start,program,[funcDef,(funcHead,["i","(","id",":","id",")","->","void","{","}","func","as"],[],"void"),funcBody],_814|_815]   Se = [start,program,[funcDef,(funcHead,["i","(","i ...",...|...],[],"void"),funcBody],_A|_B]
%@ ;  false.
%@ [start,program,[funcDef,(funcHead,["i","(",")","->","void","{","}","func","as"],[],"void"),funcBody],_787|_788]Se = [start,program,[funcDef,(funcHead,["i","(",") ...",...|...],[],"void"),funcBody],_A|_B]
%@ ;  false.
%@ d3
%@ _734d1
%@ d3
%@ _797d1
%@ d3
%@ [start,program,[funcDef,(funcHead,["i"],[],"void"),funcBody],[funcDef,(funcHead,["as"],[],"void"),funcBody],_810|_811]   Se = [start,program,[funcDef,(funcHead,["i"],[],"void"),funcBody],[funcDef,(funcHead,["as"],[],"void ..."),funcBody],_A|_B]
%@ ;  _774d1
%@ d3

%@ 
%@ caught: error('$interrupt_thrown',repl)
%@ 
%@ caught: error('$interrupt_thrown',repl)

%@ _752_804[start,program,[funcDef,(funcHead,["i"],[],"void"),funcBody],[funcDef,(funcHead,["as"],[],"void"),funcBody],_812|_813]   Se = [start,program,[funcDef,(funcHead,["i"],[],"void"),funcBody],[funcDef,(funcHead,["as"],[],"void ..."),funcBody],_A|_B], S = (_C,_D)
%@ ;  _792[start,program,[funcDef,(funcHead,["i","(",")","->","void","{","}","func","as"],[],"void"),funcBody],_800|_801]Se = [start,program,[funcDef,(funcHead,["i","(",") ...",...|...],[],"void"),funcBody],_A|_B], S = (_C,_D)
%@ ;  false.

%@ _745_792[start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_800|_801]   Se = [start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_A|_B], S = (_C,_D)
%@ ;  false.

%@ false.
%@ _745_792[start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_800|_801]   Se = [start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_A|_B], S = (_C,_D)
%@ ;  false.
%@ false.


%@ _746_792[start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_800|_801]   Se = [start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_A|_B], S = (_C,_D)
%@ ;  false.

%@ false.
%@ [start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_800|_801]   Se = [start,program,[funcDef,(funcHead,[],"void"),funcBody],[funcDef,(funcHead,[],"void"),funcBody],_A|_B], S = (_C,_D)
%@ ;  false.


% ?- phrase(start(S,Se),["func","id","(",")","->","void","{","}","func","id","(",")","->","void","{","}"]), write(Se).


% ?- phrase(id,"apple").
%@    true
%@ ;  false.
%@    true
%@ ;  false.
%@    X = ["integer"]
%@ ;  X = ["float"]
%@ ;  X = ["id"].



% AST test

ast([]) --> [].
ast([H|T]) --> whileast(H), ast(T).

whileast(while(X,Y)) --> ["while"],boolast(X), ["{"],seq(Y), ["}"],[";"].

boolast(leq(X,Y)) --> ["("],seq(X),["<="],integer(Y),[")"].
boolast(geq(X,Y)) --> ["("],seq(X),[">="],seq(Y),[")"].
boolast(eqiv(X,Y)) --> ["("],seq(X),["=="],seq(Y),[")"].

% ?- phrase(whileast(X),["while","(",Z,">=",Y,")","{",A,"}",";"]).
%@    X = while(geq([],[">=",Y]),[A]), Z = ">="
%@ ;  X = while(geq([Z],[Y]),[A])
%@ ;  X = while(geq([Z,">="],[]),[A]), Y = ">="
%@ ;  X = while(eqiv([],[">=",Y]),[A]), Z = "=="
%@ ;  X = while(eqiv([Z,">="],[]),[A]), Y = "=="
%@ ;  false.
%@    X = while(geq([],[">=",Y]),[A]), Z = ">="
%@ ;  X = while(geq([Z],[Y]),[A])
%@ ;  X = while(geq([Z,">="],[]),[A]), Y = ">="
%@ ;  X = while(eqiv([],[">=",Y]),[A]), Z = "=="
%@ ;  X = while(eqiv([Z,">="],[]),[A]), Y = "=="
%@ ;  false.;
%@ false.
%@    X = while(geq(["apple","<="],[]),"a"), Y = ">="
%@ ;  X = while(eqiv(["apple","<="],[]),"a"), Y = "=="
%@ ;  false.
%@ false.
%@ false.
%@ false.
%@    X = while(geq(["apple","<="],[]),"a"), Y = ">="
%@ ;  X = while(eqiv(["apple","<="],[]),"a"), Y = "=="
%@ ;  false.

%@    Y = ["while","(",a,p,p,l,e,"<=",'1','0','0',")","{",a,"}",";"]
%@ ;  ...
%@ false.
%@    Y = ["while","(",a,p,p,l,e,"<=",'1','0','0',")","{",a,"}",";"]
%@ caught: error('$interrupt_thrown',repl).
%@ false.

%@ false.

% ?- phrase(whileast(X), ["while","(","apples","<=","1000",")","{","apples","=","apples","+","1","}",";"]).
%@ false.
%@ false.

% ?- phrase(whileast(X), ["while","(","apples<1000",")","{","apples=apples+1","}",";"]).
%@ false.
%@    X = while(["apples<1000"],["apples=apples+1"])
%@ ;  false.

/*

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*/
% the program
prog_ast(X) --> reptprog0_ast(X).


structdecl_ast(struct(ID,Opt,RepStruct)) --> ["struct"], id(ID), optstructdecl2_ast(Opt),
					     ["{"], reptstructdecl4_ast(ReptStruct),
					     ["}"], [";"].

structorimplorfunc_ast(P) --> structdecl_ast(P).
structorimplorfunc_ast(P) --> impldef_ast(P).
structorimplorfunc_ast(P) --> funcdef_ast(P).


reptaparams1_ast(params(T,P)) --> aparamstail_ast(T), reptaparams1_ast(P).  
reptaparams1_ast([]) --> [].

reptfuncbody1_ast(function_body(B,N)) --> vardeclorstat_ast(B), reptfuncbody1_ast(N).
reptfuncbody1_ast([]) --> [].

reptimpldef3_ast(impl_def(F,N)) --> funcdef_ast(F), reptimpldef3_ast(N).   
reptimpldef3_ast([]) --> [].

reptoptstructdecl22_ast(struct(ID,N)) --> [","], id(ID), reptoptstructdecl22_ast(N).   
reptoptstructdecl22_ast([]) --> [].

reptprog0_ast(program(P,N)) --> structorimplorfunc_ast(P), reptprog0_ast(N).   
reptprog0_ast([]) --> [].

arraysize_ast(Size) --> ["["], arraysize2_ast(Size).  

arraysize2_ast(Size) --> integer(Size), ["]"]. 
arraysize2_ast([]) --> ["]"].

assignop_ast --> ["="].

% any Expression
expr_ast(expression(Expr,Rhs)) --> arithexpr_ast(Expr), expr2_ast(Rhs). 

expr2(rhs(Rel,Arith)) --> relop_ast(R), arithexpr(Arith).  
expr2([]) --> [].

			       
% array parameters
aparams_ast([H|T]) --> expr_ast(H), reptaparams1_ast(T).
aparams_ast([]) --> [].

% array paramters tail
aparamstail_ast(X) --> [","], expr_ast(X).  

% arithmetic expressions
arithexpr_ast(arith_expr(Term,Rhs)) --> term_ast(Term), rightrecarithexpr_ast(Rhs).


rightrecarithexpr_ast([]) -->  [].
rightrecarithexpr_ast(rhs(A,T,N)) --> addop_ast(A), term_ast(T), rightrecarithexpr_ast(T).

rightrecterm([]) -->  [].
rightrecterm(right_term(Factor,Rhs)) -> multop_ast, factor_ast(F), rightrecterm_ast(Rhs).   
			  
term_ast(term(Term,Rhs)) --> factor_ast(F), rightrecterm_ast(Rhs).   

factor_ast(factor(F)) --> id(I), factor2_ast(F2), reptvariableorfunctioncall_ast(RVOFC).  
factor_ast(factor(F)) --> integer(I).  
factor_ast(factor(F)) --> float(F).
factor_ast(factor(F)) --> ["("], arithexpr_ast(F), [")"].
factor_ast(factor(F)) --> ["!"], factor_ast(F).
factor_ast(factor(F)) --> sign_ast, factor_ast(F).

factor2_ast(F2) --> ["("], aparams_ast(F2), [")"].
factor2_ast(F2) --> reptidnest1(F2).  

fparams_ast(function_parameter(ID,Type,Rept3,Rept4)) --> id(ID), [":"], type_ast(Type),
						     reptfparams3_ast(Rept3),
						     reptfparams4_ast(Rept4).  
fparams_ast(function_parameter([])) --> [].

fparamstail(function_parameter_tail(ID,Type,ReptTail)) --> [","], id(ID), [":"], type_ast(Type),
							 reptfparamstail4_ast(ReptTail).

reptfparams3_ast(repeated_params3(Size)) --> arraysize_ast(Size), reptfparams3_ast(Rept3).  
reptfparams3_ast([]) --> [].

reptfparams4_ast(repeated_params4(Tail,Param)) --> fparamstail_ast(Tail), reptfparams4_ast(Param).
reptfparams4_ast([]) --> [].

reptfparamstail4_ast(repeated_param_tail(Size,Param)) --> arraysize_ast(Size), reptfparamstail4_ast(ParamTail).  
reptfparamstail4_ast([]) --> [].


funcbody_ast(function_body(Body)) --> ["{"], reptfuncbody1_ast(Body), ["}"].

funcdecl_ast(function_declaration(F)) --> funchead_ast(F) ,[";"].  

funcdef_ast(function_definition(Head,Body)) --> funchead_ast(Head), funcbody_ast(Body).  

funchead_ast(function_head(ID,Params,ReturnType)) --> ["func"], id(ID),
			       ["("], fparams_ast(Params), [")"],
			       ["->"], returntype_ast(Type).  

statement_ast(statememt_id(ID,Idnest)) --> id(ID), statementidnest_ast(Idnest), [";"]. 
statement_ast(if(Expr,StatblockIf,StatblockElse)) --> ["if"], ["("], relexpr_ast(Expr), [")"], ["then"],
		    statblock_ast(StatblockIf), ["else"],
		    statblock_ast(StatblockElse), [";"].  
statement_ast(while(Expr,Statblock)) --> ["while"], ["("], relexpr_ast(Expr), [")"],
					 statblock_ast(Statblock), [";"].  
statement_ast(read(Variable)) --> ["read"], ["("], variable_ast(Variable), [")"], [";"].  
statement_ast(write(Expr)) --> ["write"], ["("], expr_ast(Expr), [")"], [";"].  
statement_ast(return(Expr)) --> ["return"], ["("], expr_ast(Expr), [")"], [";"].  


statementidnest_ast(statement_id(ID,Idnest)) --> ["."], id(ID), statementidnest_ast(Idnest).
statementidnest_ast(statement_id(Params,Idnest)) --> ["("], aparams_ast(Params), [")"], statementidnest2_ast(Idnest) .
statementidnest_ast(statement_id(Indice,Idnest,StatementIdNest)) --> indice_ast(Indice), reptidnest1_ast(Idnest), statementidnest3(StatmentIdNest).
statementidnest_ast(assign(Expr)) --> assignop_ast, expr_ast(Expr).

statementidnest2_ast([]) --> [].
statementidnest2_ast(statement_id(ID,IDnest)) --> ["."], id(ID), statementidnest_ast(IDnest).  

statementidnest3(assign(Expr)) --> assignop_ast, expr_ast(Expr).
statementidnest3(statement_id(ID,IDnest)) --> ["."], id(ID), statementidnest_ast(IDnest).  

reptidnest1_ast(idnest(Indice,N)) --> indice_ast(Indice), reptidnest1_ast(N).  
reptidnest1_ast([]) --> [].

reptvariableorfunctioncall_ast(id_nest(ID,N)) -->  idnest_ast(ID), reptvariableorfunctioncall_ast(N).  
reptvariableorfunctioncall_ast([]) -->  [].

idnest_ast(idnest(ID,N)) --> ["."], id(ID), idnest2_ast(N).  
idnest2_ast(param(Param)) --> ["("], aparams_ast(Param), [")"].  
idnest2_ast(N) --> reptidnest1_ast(N).  


impldef_ast(impl_def(ID,Impl)) --> ["impl"], id(ID), ["{"], reptimpldef3_ast(Impl), ["}"].  

indice_ast(indice(Indice)) --> ["["], arithexpr_ast(Indice), ["]"].  

memberdecl_ast(member(Fun)) --> funcdecl_ast(Fun).  
memberdecl_ast(var(Var)) --> vardecl_ast(Var).  

optstructdecl2_ast(struct(ID,N)) --> ["inherits"], id(ID), reptoptstructdecl22_ast(N).  
optstructdecl2_ast([]) --> [].

relexpr_ast(relational_expr(Arith1,Rel,Arith2)) --> arithexpr_ast(Arith1), relop_ast(Rel), arithexpr_ast(Arith2).

reptstatblock1_ast(statblock(S,N)) --> statement_ast(S), reptstatblock1_ast(N).   
reptstatblock1_ast([]) --> [].

reptstructdecl4_ast(struct(Vis,Mem,N)) --> visibility_ast(Vis),
				       memberdecl_ast(Mem),
				       reptstructdecl4_ast(N).   
reptstructdecl4_ast([]) --> [].

reptvardecl4_ast(var(Size,N)) --> arraysize_ast(Size), reptvardecl4_ast(N).   
reptvardecl4_ast([]) --> [].

		 
statblock_ast(statblock(S)) --> ["{"], reptstatblock1_ast(S), ["}"].   
statblock_ast(statblock(S)) --> statement_ast(S).   
statblock_ast([]) -->  [].

vardecl_ast(var(ID,Type,N)) --> ["let"], id(ID), [":"], type_ast(Type), reptvardecl4_ast(N), [";"].   

vardeclorstat_ast(V) --> vardecl_ast(V).   
vardeclorstat_ast(S) --> statement_ast(S).   

variable_ast(var(ID,N)) -->  id(ID), variable2_ast(N).  

variable2_ast(var(Idnest,N)) -->  reptidnest1_ast(Idnest), reptvariable_ast(N).    
variable2_ast(var(Param,Idnest)) -->  ["("], aparams_ast(Param), [")"], varidnest_ast(Idnest).

reptvariable_ast(repeated_var(V,N)) -->  varidnest_ast(V), reptvariable_ast(N).   
reptvariable_ast([]) --> [].

varidnest_ast(var(ID,N)) --> ["."], id(ID), varidnest2_ast(N).
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

sign_ast --> ["+"].
sign_ast --> ["-"].


		       
type_ast("integer") --> ["integer"].
type_ast("float") --> ["float"].
type_ast("id") --> ["id"].


% addition operators 
addop_ast("+") --> ["+"].
addop_ast("-") --> ["-"].
addop_ast("|") --> ["|"].

% multiplication operators
multop_ast("*") --> ["*"].
multop_ast("/") --> ["/"].
multop_ast("&") --> ["&"].

visibility_ast(visibility("public")) --> ["public"].  
visibility_ast(visibility("private")) -->["private"].


% ?- phrase(prog_ast(X),["func",Y,"(",")","->","void","{","}","func",A1,"(",")","->","void","{","}"]).
%@    X = program(function_definition(function_head("a",function_parameter([]),_A),function_body([])),program(function_definition(function_head("a",function_parameter([]),_B),function_body([])),[])), Y = a, A1 = a



% ?- phrase(prog_ast(X),["func",Y,"(",")","->","void","{","}","func","as","(",")","->","void","{","}"]).



ast_generation(Filein,Fileout) :-
    open(Filein,read,S1,[]),
    open(Fileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst),
    write(Lst),
    close(S1),
    close(S2).

% ?- 
