/*===========================================================
Author: Alexander De Laurentiis
Compiler for an object oriented language

===========================================================*/

:-use_module(library(files)).
:-use_module(library(charsio)).
:-use_module(library(clpz)).
:-use_module(library(dcgs)).
:-use_module(library(lists)).
:-use_module(library(builtins)).
:-use_module(library(terms)).
:-use_module(library(gensym)).
:-use_module(library(http/http_open)).
:-use_module(library(xpath)).
:-use_module(library(sgml)).



:- dynamic(symbol/4).
:- dynamic(symbol/5).
:- dynamic(symbol/6).
:- dynamic(inherits/2).
:- dynamic(expression/2).
:- dynamic(expr/1).
:- dynamic(scope_code/2).
:- dynamic(cell/3).
:- dynamic(reserved/4).






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
token("//", 'Inline Comment').
token("/*", 'Comment start').
token("*/", 'Comment end').
token(X, 'Float'):-float_(X).
token(X, 'ID'):-id(X).
token(X, 'Integer'):-integer_(X).


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
    analyze(T,L,_Cl,O),!.


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
    write(Lst),nl,
    analyze(Lst,0,0,S2),
    close(S1),
    close(S2).

/*===========================================================

The execution functions for usage of ediprolog in emacs, the 
best way in which to program in scryer-prolog.

===========================================================*/


% ?- test_negative.
%@ false.


% ?- test_positive.
%@ false.
%@    true
%@ ;  false.


% 
% ?- analyze_file("bubblesort.src","bubblesort_semantic_analyzed.txt").
%@ ["//","Assignment","5","coverage",":","
%@ ","//","-","-","-","-","-","-","-","-","-","-","-","-","-","
%@ ","//","|","YES","|","NO","|","
%@ ","//","-","-","-","-","-","-","-","-","-","-","-","-","-","
%@ ","//","1",".","1",":","|","X","|","|","
%@ ","//","1",".","2",":","|","X","|","|","
%@ ","//","1",".","3",":","|","|","X","|","
%@ ","//","1",".","4",":","|","|","X","|","
%@ ","//","2",".","1",":","|","X","|","|","
%@ ","//","2",".","2",":","|","X","|","|","
%@ ","//","2",".","3",":","|","|","X","|","
%@ ","//","2",".","4",":","|","|","X","|","
%@ ","//","3",".","1",":","|","X","|","|","
%@ ","//","3",".","2",":","|","X","|","|","
%@ ","//","3",".","3",":","|","X","|","|","
%@ ","//","3",".","4",":","|","X","|","|","
%@ ","//","4",".","1",":","|","X","|","|","
%@ ","//","4",".","2",":","|","|","X","|","
%@ ","//","4",".","3",":","|","|","X","|","
%@ ","//","4",".","4",":","|","|","X","|","
%@ ","//","5",".","1",":","|","X","|","|","
%@ ","//","5",".","2",":","|","X","|","|","
%@ ","//","5",".","3",":","|","|","X","|","
%@ ","//","-","-","-","-","-","-","-","-","-","-","-","-","-","
%@ ","
%@ ","/*","sort","the","array","*/","
%@ ","func","bubbleSort","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","//","2",".","1",",","2",".","2","
%@ ","{","
%@ ","let","n",":","integer",";","//","1",".","1","
%@ ","let","i",":","integer",";","//","1",".","1","
%@ ","let","j",":","integer",";","//","1",".","1","
%@ ","let","temp",":","integer",";","//","1",".","1","
%@ ","n","=","size",";","//","3",".","1","
%@ ","i","=","0",";","//","3",".","1","
%@ ","j","=","0",";","//","3",".","1","
%@ ","temp","=","0",";","//","3",".","1","
%@ ","while","(","i","<","n","-","1",")","{","//","3",".","3",",","5",".","1","
%@ ","j","=","0",";","//","3",".","1","
%@ ","while","(","j","<","n","-","i","-","1",")","{","//","3",".","3","
%@ ","if","(","arr","[","j","]",">","arr","[","j","+","1","]",")","//","3",".","2",",","4",".","1",",","5",".","1",",","5",".","2","
%@ ","then","{","
%@ ","//","swap","temp","and","arr","[","i","]","
%@ ","temp","=","arr","[","j","]",";","//","3",".","1",",","4",".","1",",","5",".","2","
%@ ","arr","[","j","]","=","arr","[","j","+","1","]",";","//","3",".","1",",","4",".","1",",","5",".","2","
%@ ","arr","[","j","+","1","]","=","temp",";","//","3",".","1",",","4",".","1",",","5",".","2","
%@ ","}","else",";","
%@ ","j","=","j","+","1",";","//","3",".","1",",","5",".","1","
%@ ","}",";","
%@ ","i","=","i","+","1",";","//","3",".","1",",","5",".","1","
%@ ","}",";","
%@ ","}","
%@ ","
%@ ","/*","/*","/*","apples","*/","*/","Print","the","array","*/","
%@ ","func","printArray","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","//","2",".","1",",","2",".","2","
%@ ","{","
%@ ","let","n",":","integer",";","//","1",".","1","
%@ ","let","i",":","integer",";","//","1",".","1","
%@ ","n","=","size",";","//","3",".","1","
%@ ","i","=","0",";","//","3",".","1","
%@ ","while","(","i","<","n",")","{","//","3",".","3",",","5",".","1","
%@ ","write","(","arr","[","i","]",")",";","//","3",".","4",",","4",".","1",",","5",".","2","
%@ ","i","=","i","+","1",";","//","3",".","1",",","5",".","1","
%@ ","}",";","
%@ ","}","
%@ ","
%@ ","//","main","funtion","to","test","above","
%@ ","func","main","(",")","->","void","
%@ ","{","
%@ ","let","arr",":","integer","[","7","]",";","//","1",".","1","
%@ ","arr","[","0","]","=","64",";","//","3",".","1","
%@ ","arr","[","1","]","=","34",";","//","3",".","1","
%@ ","arr","[","2","]","=","25",";","//","3",".","1","
%@ ","arr","[","3","]","=","12",";","//","3",".","1","
%@ ","arr","[","4","]","=","22",";","//","3",".","1","
%@ ","arr","[","5","]","=","11",";","//","3",".","1","
%@ ","arr","[","6","]","=","90",";","//","3",".","1","
%@ ","printarray","(","arr",",","7",")",";","//","2",".","1",",","2",".","2","
%@ ","/*","assumes","that","the","array","is","passed","as","a","reference","*/","
%@ ","bubbleSort","(","arr",",","7",")",";","//","2",".","1",",","2",".","2","
%@ ","printarray","(","arr",",","7",")",";","//","2",".","1",",","2",".","2","
%@ ","}"]
%@ "inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment""inline comment"   true
%@ ;  ...


% ?- open("t1.nl",read,S,[]),read(S,L),close(S).
%@    S = '$stream'(0x5568b2269ad0), L = "if 3 <> 2\nthen\n\tv : ...".
%@    S = '$stream'(0x5568b2269ad0), L = "\n\nfunc main() -> vo ...".


read(Stream,[]):-at_end_of_stream(Stream),!.
read(Stream,[H|T]):-
    get_char(Stream,H),
    read(Stream,T).



/*===========================================================

RegExes for each of the specified lexical elements to construct each
and identify it if needed.

===========================================================*/

% non zero digits
nonzero('1').
nonzero('2').
nonzero('3').
nonzero('4').
nonzero('5').
nonzero('6').
nonzero('7').
nonzero('8').
nonzero('9').

% digits
digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

% Lowercase
letter('a').
letter('b').
letter('c').
letter('d').
letter('e').
letter('f').
letter('g').
letter('h').
letter('i').
letter('j').
letter('k').
letter('l').
letter('m').
letter('n').
letter('o').
letter('p').
letter('q').
letter('r').
letter('s').
letter('t').
letter('u').
letter('v').
letter('w').
letter('x').
letter('y').
letter('z').

% Uppercase
letter('A').
letter('B').
letter('C').
letter('D').
letter('E').
letter('F').
letter('G').
letter('H').
letter('I').
letter('J').
letter('K').
letter('L').
letter('M').
letter('N').
letter('O').
letter('P').
letter('Q').
letter('R').
letter('S').
letter('T').
letter('U').
letter('V').
letter('W').
letter('X').
letter('Y').
letter('Z').

alphanum('_').
alphanum(C) :- letter(C).
alphanum(C) :- digit(C).


character(C) :- alphanum(C).
character(' ').



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

% ?- fraction(".0").
%@    true.

% ?- float_("123.01323e+03").
%@ false.
%@ false.
%@ false.
%@ false.
% ?- float_("123.01323e-833").
%@    true.
%@    true.
%@    true.
% ?- float_("1.01323e-833").
%@    true.
%@    true.
%@    true.
% ?- float_(".01323e-833").
%@ false.
%@ false.
%@ false.
% ?- float_("123.01323e+3").
%@    true.
%@    true.
% ?- float_("123.01323e+30").
%@    true.
%@    true.
% ?- float_("123.01323e+03").
%@ false.
%@ false.
% ?- float_("0123.01323e+3").
%@ false.
%@ false.
% ?- float_("1.23").
%@    true.
% ?- float_("120.34e10").
%@    true.
%@    true.
% ?- float_("10.0").
%@    true.
%@    true.
%@    true.
%@ false.
%@ false.
%@ false.
%@    true.

%?- float_(X).
%@    X = "0.0".
%@    X = "0.0".


id([H|T]) :-
    letter(H),
    id0(T).
id0([]).
id0([H|T]) :-
    alphanum(H),
    id0(T).


% ?- id("ddd2s23").
%@    true
%@ ;  ...
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
strip_comments([_H|T],Cl0,Out):-
    C10 #> 0,
    strip_comments(T,Cl0,Out).
strip_comments([H|T],Cl0,Out):-
    Cl0 #= 0,
    H = "//",
    split_delimiter([H|T],_Comment,Rest,"\n"),
    strip_comments(Rest,Cl0,Out).
strip_comments([H|T],Cl0,Out):-
    Cl0 #= 0,
    token(H,D),
    D = 'Comment start',
    Cl #= Cl0 + 1,
    strip_comments(T,Cl,Out).
strip_comments([H|T1],Cl0,[H|T2]):-
    Cl0 #= 0,
    strip_comments(T1,Cl0,T2).

remove_comment_blocks([H|T]) --> [H], ["/*"], seq(_) ,["*/"], remove_comment_blocks(T).
remove_comment_blocks([H|T]) --> ["/*"], seq(_) ,["*/"],[H], remove_comment_blocks(T).
remove_comment_blocks([H|T]) --> [H], remove_comment_blocks(T).
remove_comment_blocks([]) --> [].


remove_inline([],[]).
remove_inline([H|T],L) :-
    H = "//",
    split_delimiter([H|T],_Comment,Rest,['\n']),
    remove_inline(Rest,L),!.
remove_inline([H|T],[H|T2]) :-
    remove_inline(T,T2),!.


/* Data Structures  */


dictionary(void).
dictionary(dict(_,_,D1,D2)):-
    dictionary(D1),
    dictionary(D2).

% dict(Key, Value, Left, Right).

lookup(Key,dict(Key,X,_Left,_Right),Value):-
    !,
    X = Value.
lookup(Key,dict(Key1,_X,Left,_Right),Value):-
    Key @< Key1,
    lookup(Key,Left,Value).
lookup(Key,dict(Key1,_X,_Left,Right),Value):-
    Key @> Key1,
    lookup(Key,Right,Value).


% ?- lookup(app, X,red),lookup(3,X,yellow),lookup(1, X,ee),lookup(7,X,www).
%@    X = dict(app,red,dict(3,yellow,dict(1,ee,_A,_B),dict(7,www,_C,_D)),_E)
%@ ;  false.



/*===========================================================

DCG's written to parse the value into a tokenized list from the
top down with syntax to easily accomodate expanding delimiters.

===========================================================*/


wordlist([X,Z|Y]) --> word(X), whitespace(Z), wordlist(Y).
wordlist([X,Z]) --> whitespace(X), wordlist(Z).
wordlist([X,_Z]) --> word(X).
wordlist([X,Z]) --> word(X), whitespace(Z).

% describes sequence of any characters
... --> [] | [_], ... .


% A sequence of characters
seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

% for current readability but likely to change
%word(X) --> X,{token(X,_)}.
word(X) --> seq(X).

% definitions of whitespace or delimiters
% Unique Case for idnests vs floats
%whitespace((I1,'.',I2)) --> {integer_(I1),integer_(I2)},I1,'.',I2.
%whitespace([I,'.']) --> {I='0'},"0",".".
%whitespace([I,'.']) --> {integer_(I)},I,".".

whitespace(".") --> ".".

whitespace(".") --> ".".
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
whitespace(",") --> ",".
% Equality
whitespace("==") --> "==".
whitespace("!=") --> "!=".
whitespace(">=") --> ">=".
whitespace("<=") --> "<=".
whitespace(">") --> ">".
whitespace("<") --> "<".
whitespace("\\=") --> "\\=".
whitespace("!") --> "!".
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
whitespace(X) -->{X=[]}, " ", whitespace(_Z).
whitespace(X) -->{X=[]}, "\t", whitespace(_Z).
whitespace(X) -->{X=[]}, "\r", whitespace(_Z).


fix_floats([],[]).
fix_floats([H1,".",H2|T1],[H|T2]):-
    integer_(H1),
    integer_(H2),
    treple(H1,".",H2,H),
    fix_floats(T1,T2).
fix_floats([H1,".",H2|T1],[H|T2]):-
    integer_(H1),
    split_delimiter(H2,F,['e'|E],'e'),
    fraction(['.'|F]),
    integer_(E),
    append(H1,['.'|H2],H),
    fix_floats(T1,T2).
fix_floats([H1,".",H2,"+",H3|T1],[H|T2]):-
    integer_(H1),
    split_delimiter(H2,F,['e'],'e'),
    fraction(['.'|F]),
    integer_(H3),
    treple(H1,['.'|H2],['+'|H3],H),
    fix_floats(T1,T2).
fix_floats([H1,".",H2,"-",H3|T1],[H|T2]):-
    integer_(H1),
    split_delimiter(H2,F,['e'],'e'),
    fraction(['.'|F]),
    integerr_(H3),
    treple(H1,['.'|H2],['-'|H3],H),
    fix_floats(T1,T2).
fix_floats([H1|T1],[H1|T2]):-
    fix_floats(T1,T2).





% ?- phrase(float(X),"  arr[0] = ").
%@ 
%@ caught: error('$interrupt_thrown',repl)


% ?- phrase((...,I1,'.',I2,...),["252",".","032e5",";","0",".","932e","+","32",";","arr1",".","arr","[","1","]","=","34",";"]).
%@ caught: error(type_error(callable,["252",".","032e5",";","0",".","932e","+","32",";","arr1",".","arr","[","1","]","=","34",";"]),'(:)'/2)


% a test case
% ?- phrase(wordlist(X),"==\t+\t\n|\t(\t;\tif\n \t\t \t"),!,remove(X,[],Lst).
%@ 
%@ caught: error('$interrupt_thrown',repl)
%@    X = [[],"==",[],[],[],"+",[],[],[],...|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.

% ?- phrase(wordlist(X),"  arr[0] = 0.0;arr[1] = -34.0;252.032e5; 0.932e+32;arr1.arr[1] = 34;"),!,remove(X,[],Lst),fix_floats(Lst,L),!,write(L).
%@ ["arr","[","0","]","=","0.0",";","arr","[","1","]","=","-","34.0",";","252.032e5",";","0.932e+32",";","arr1",".","arr","[","1","]","=","34",";"]





% ?- phrase(wordlist(X),"if 3 <> 2 then v := true ;w := false else v := true endif"),!,remove(X,[],Lst),write(Lst).
%@ ["if","3","<",">","2","then","v",":","=","true",";","w",":","=","false","else","v",":","=","true","endif"]   X = ["if",[],"3",[],[],"<",[],">",[],[]|...], Lst = ["if","3","<",">","2","then","v",":","= ...",...|...]
%@ ;  false.


% ?- float_("0.92e+32").
%@    true.

% ?- float_("34.0").
%@    true.

%?- float_("252.032e5"). 
%@    true.
% ?- float_("0.932e+32").
%@    true.

/* 

Idea for error correction, have a symbol for every symbol that will be proposed missing and print an error if missing when parsing.

*/

/*
------------------------------------------------------------------------------------------

Parser

idea

DO THIS WITH THE \n IN TO KEEP A LINE COUNT

parser([],_Lc,[],).
parser([Current,Next|T1],Lc,[Transition|T2]):-
  transition(Current,Next,Transition),
  parser([Next|T1],Lc,T2).
parser([Current,'\n'|T1],0Lc,[Transition|T2]):-
  Lc #= 0Lc + 1,
  parser([Current|T1],T2).
parser([Current,Next|T1],Lc,[Transition|T2]):-
  write(["Iccorrect value after char",Current,Next,Lc]),nl,
  parser([Next|T1],Lc,T2).

transition(Token1,Token2,Transition).

------------------------------------------------------------------------------------------
*/


/*
URL FOR THE TABLE
-----------------------------------------------------------------------------
https://smlweb.cpsc.ucalgary.ca/ll1-table.php?grammar=START+-%3E+REPTPROGRAM+.%0A%0AREPTPROGRAM+-%3E+STRUCTORIMPLORFUNC+REPTPROGRAM%0A%7C+.%0A%0A%0AADDOP+-%3E+plus+%0A++++++++%7Cminus+%0A++++++++%7Cor+.%0A%0AAPARAMS+-%3E+EXPR+APARAMSTAIL+%0A++++++++++%7C.%0A%0AAPARAMSTAIL+-%3E+comma+EXPR+.%0A%0AARITHEXPR+-%3E+TERM+RIGHTRECARITHEXPR+.%0A%0AARRAYSIZE+-%3E+lsqbr+ARRAYSIZEP+rsqbr+.%0A%0AARRAYSIZEP+-%3E+intnum+%0A+++++++++++++%7C.%0A%0AASSIGNOP+-%3E+equal+.%0A%0AEXPR+-%3E++ARITHEXPR+EXPR2.%0A%0AEXPR2+-%3E+RELOP+ARITHEXPR%0A++++++%7C.%0A%0AFACTOR+-%3E+id+FACTOR2+REPTVARIABLEORFUNCTIONCALL+%0A+++++++++%7Cintlit+%0A+++++++++%7Cfloatlit+%0A+++++++++%7Clpar+ARITHEXPR+rpar+%0A+++++++++%7Cnot+FACTOR+%0A+++++++++%7CSIGN+FACTOR+.%0A%0AFACTOR2+-%3E+lpar+APARAMS+rpar%0A%09%7C+REPTIDNEST1+.%0A%0AREPTVARIABLEORFUNCTIONCALL+-%3E+IDNEST+REPTVARIABLEORFUNCTIONCALL%0A%09%09%09+++%7C+.%0A%0A%0AFPARAMS+-%3Eid+colon+TYPE+REPTFPARAMS3+REPTFPARAMS4%0A++++++++++%7C.%0A%0A%0AREPTFPARAMS3+-%3E%09ARRAYSIZE+REPTFPARAMS3%0A%09+++++%7C+.%0A%0A%0AREPTFPARAMS4+-%3E+FPARAMSTAIL+REPTFPARAMS4%0A%09+++++%7C+.%0A%0A%0AREPTFPARAMSTAIL4+-%3E+ARRAYSIZE+REPTFPARAMSTAIL4%0A%09+++++%7C+.%0A%0A%0AFPARAMSTAIL+-%3E+comma+id+colon+TYPE+REPTFPARAMSTAIL4+.%0A%0AFUNCBODY+-%3E+lcurbr+REPTFUNCBODY+rcurbr+.%0A%0AREPTFUNCBODY+-%3E+VARDECLORSTAT+REPTFUNCBODY%0A%7C.%0A%0A%0AFUNCDECL+-%3E+FUNCHEAD+semi+.%0A%0AFUNCDEF+-%3E+FUNCHEAD+FUNCBODY+.%0A%0AFUNCHEAD+-%3E+func+id+lpar+FPARAMS+rpar+minusarrow+RETURNTYPE+.%0A%0AIDNEST+-%3E++dot+id+IDNEST2+.%0A%0AIDNEST2+-%3E+lpar+APARAMS+rpar%0A++++++%7CREPTIDNEST1+.%0A%0AIMPLDEF+-%3E+impl+id+lcurbr+REPTFUNCDEF+rcurbr+.%0A%0AREPTFUNCDEF+-%3E+FUNCDEF+REPTFUNCDEF%0A%09++++%7C+.%0A%0AINDICE+-%3E+lsqbr+ARITHEXPR+rsqbr+.%0A%0AMEMBERDECL+-%3E+FUNCDECL+%0A+++++++++++++%7CVARDECL+.%0A%0AMULTOP+-%3E+mult+%0A+++++++++%7Cdiv+%0A+++++++++%7Cand+.%0A%0ASTRUCTINHERITS+-%3E+inherits+id+REPTSTRUCTINHERITS+%0A+++++++++++++++++%7C.%0A%0AREPTSTRUCTINHERITS+-%3E+comma+id+REPTSTRUCTINHERITS%0A%09%09++%7C+.%0A%0A%0A%0ARELEXPR+-%3E+ARITHEXPR+RELOP+ARITHEXPR+.%0A%0ARELOP+-%3E+eq+%0A++++++++%7Cneq+%0A++++++++%7Clt+%0A++++++++%7Cgt+%0A++++++++%7Cleq+%0A++++++++%7Cgeq+.%0A%0ARETURNTYPE+-%3E+TYPE+%0A+++++++++++++%7Cvoid+.%0A%0ARIGHTRECARITHEXPR+-%3E+%0A++++++++++++++++++++%7CADDOP+TERM+RIGHTRECARITHEXPR+.%0A%0ARIGHTRECTERM+-%3E+%0A+++++++++++++++%7CMULTOP+FACTOR+RIGHTRECTERM+.%0A%0A%0ASIGN+-%3E+plus+%0A+++++++%7Cminus+.%0A%0ASTATBLOCK+-%3E+lcurbr+STATEMENT+rcurbr+%0A++++++++++++%7CSTATEMENT+%0A++++++++++++%7C.%0A%0ASTATEMENT+-%3E+id+STATEMENTIDNEST+semi%0A++++++++++++%7Cif+lpar+RELEXPR+rpar+then+STATBLOCK+else+STATBLOCK+semi+%0A++++++++++++%7Cwhile+lpar+RELEXPR+rpar+STATBLOCK+semi+%0A++++++++++++%7Cread+lpar+VARIABLE+rpar+semi+%0A++++++++++++%7Cwrite+lpar+EXPR+rpar+semi+%0A++++++++++++%7Creturn+lpar+EXPR+rpar+semi.%0A%0ASTATEMENTIDNEST+-%3E+dot+id+statementidnest%0A%7C+lpar+APARAMS+rpar+STATEMENTIDNEST2%0A%7C+INDICE+REPTIDNEST1+STATEMENTIDNEST3%0A%7C+ASSIGNOP+EXPR+.%0A%0ASTATEMENTIDNEST2+-%3E+dot+id+STATEMENTIDNEST+%0A%7C+.%0A%0ASTATEMENTIDNEST3+-%3E+ASSIGNOP+EXPR%0A%7C+dot+id+STATEMENTIDNEST+.%0A%0AREPTIDNEST1+-%3E+INDICE+REPTIDNEST1%0A%09++++%7C+.%0A%0A%0A%0A%0ASTRUCTDECL+-%3E+struct+id+STRUCTINHERITS+lcurbr+REPTSTRUCTDECL+rcurbr+semi+.%0A%0AREPTSTRUCTDECL+-%3E+VISIBILITY+MEMBERDECL+REPTSTRUCTDECL%0A%09+++++++%7C.%0A%0A%0ASTRUCTORIMPLORFUNC+-%3E+STRUCTDECL+%0A+++++++++++++++++++++%7CIMPLDEF+%0A+++++++++++++++++++++%7CFUNCDEF+.%0A%0ATERM+-%3E+FACTOR+RIGHTRECTERM+.%0A%0ATYPE+-%3E+integer+%0A+++++++%7Cfloat+%0A+++++++%7Cid+.%0A%0AVARDECL+-%3E+let+id+colon+TYPE+REPTVARDECL+semi+.%0A%0AREPTVARDECL+-%3E+ARRAYSIZE+REPTVARDECL%0A%09+++%7C+.%0A%0AVARDECLORSTAT+-%3E+VARDECL+%0A++++++++++++++++%7CSTATEMENT+.%0A%0AVARIABLE+-%3E++id+VARIABLE2.%0A%0AVARIABLE2+-%3E+REPTIDNEST1+REPTVARIABLE+%0A%09++%7C+lpar+APARAMS+rpar+VARIDNEST.%0A%0AREPTVARIABLE+-%3E+VARIDNEST+REPTVARIABLE%0A%09+++++%7C+.%0A%0AVARIDNEST+-%3E+dot+id+VARIDNEST2+.%0A%0AVARIDNEST2+-%3E+lpar+APARAMS+rpar+VARIDNEST%0A%09+++%7C+REPTIDNEST1+.%0A%0AVISIBILITY+-%3E+public+%0A+++++++++++++%7Cprivate+.%0A&substs=
-----------------------------------------------------------------------------

*/


/*

modifications to
- funcbody
  - Added reptfuncbody
- arraysize
  - added terminal
- statement
 - added
   - statementidnest
   - reptidnest
- reptfparams
- idnest

*/


% transition(current_node,next_node,[firstset]).
transition(start,'$',[reptprogram]).
transition(start,"struct",[reptprogram]).
transition(start,"impl",[reptprogram]).
transition(start,"func",[reptprogram]).
transition(reptprogram,"struct",[structorimplorfunc,reptprogram]).
transition(reptprogram,"impl",[structorimplorfunc,reptprogram]).
transition(reptprogram,"func",[structorimplorfunc,reptprogram]).
transition(reptprogram,'$',[]).
transition(addop,"-",["-"]).
transition(addop,"+",["+"]).
transition(addop,"|",["|"]).
transition(aparms,id,[expr,aparamstail]).
transition(aparms,")",[]).
transition(aparms,"(",[expr,aparamstail]).
transition(aparms,"-",[expr,aparamstail]).
transition(aparms,"+",[expr,aparamstail]).
transition(aparms,"!",[expr,aparamstail]).
transition(aparms,floatnum,[expr,aparamstail]).
transition(aparms,intnum,[expr,aparamstail]).
transition(aparamstail,",",[",",expr]).
transition(arithexpr,id,[term,rightrecarithexpr]).
transition(arithexpr,"(",[term,rightrecarithexpr]).
transition(arithexpr,"-",[term,rightrecarithexpr]).
transition(arithexpr,"+",[term,rightrecarithexpr]).
transition(arithexpr,"!",[term,rightrecarithexpr]).
transition(arithexpr,floatnum,[term,rightrecarithexpr]).
transition(arithexpr,intnum,[term,rightrecarithexpr]).
transition(arraysize,"[",["[",arraysizep,"]"]).
transition(arraysizep,"]",[]).
transition(arraysizep,intnum,[intnum]).
transition(assignop,"=",["="]).
transition(expr,id,[arithexpr,expr2]).
transition(expr,"(",[arithexpr,expr2]).
transition(expr,"+",[arithexpr,expr2]).
transition(expr,"-",[arithexpr,expr2]).
transition(expr,"!",[arithexpr,expr2]).
transition(expr,floatnum,[arithexpr,expr2]).
transition(expr,intnum,[arithexpr,expr2]).
transition(expr2,";",[]).
transition(expr2,")",[]).
transition(expr2,",",[]).
transition(expr2,">=",[relop,arithexpr]).
transition(expr2,"<=",[relop,arithexpr]).
transition(expr2,">",[relop,arithexpr]).
transition(expr2,"<",[relop,arithexpr]).
transition(expr2,"!=",[relop,arithexpr]).
transition(expr2,"==",[relop,arithexpr]).
transition(factor,intnum,[intnum]).
transition(factor,floatnum,[floatnum]).
transition(factor,"!",["!",factor]).
transition(factor,"-",[sign,factor]).
transition(factor,"+",[sign,factor]).
transition(factor,"(",["(",arithexpr,")"]).
transition(factor,id,[id,factor2,reptvariableorfunctioncall]).
transition(factor2,")",[reptidnest]).
transition(factor2,".",[reptidnest]).
transition(factor2,";",[reptidnest]).
transition(factor2,"(",["(",aparams,")"]).
transition(factor2,"+",[reptidnest]).
transition(factor2,"-",[reptidnest]).
transition(factor2,">=",[reptidnest]).
transition(factor2,"<=",[reptidnest]).
transition(factor2,">",[reptidnest]).
transition(factor2,"<",[reptidnest]).
transition(factor2,"!=",[reptidnest]).
transition(factor2,"==",[reptidnest]).
transition(factor2,",",[reptidnest]).
transition(factor2,"&",[reptidnest]).
transition(factor2,"|",[reptidnest]).
transition(factor2,"/",[reptidnest]).
transition(factor2,"*",[reptidnest]).
transition(factor2,"]",[reptidnest]).
transition(factor2,"[",[reptidnest]).
transition(reptvariableorfunctioncall,")",[]).
transition(reptvariableorfunctioncall,".",[idnest,reptvariableorfunctioncall]).
transition(reptvariableorfunctioncall,";",[]).
transition(reptvariableorfunctioncall,"-",[]).
transition(reptvariableorfunctioncall,"+",[]).
transition(reptvariableorfunctioncall,">=",[]).
transition(reptvariableorfunctioncall,"<=",[]).
transition(reptvariableorfunctioncall,">",[]).
transition(reptvariableorfunctioncall,"<",[]).
transition(reptvariableorfunctioncall,"!=",[]).
transition(reptvariableorfunctioncall,"==",[]).
transition(reptvariableorfunctioncall,",",[]).
transition(reptvariableorfunctioncall,"&",[]).
transition(reptvariableorfunctioncall,"|",[]).
transition(reptvariableorfunctioncall,"/",[]).
transition(reptvariableorfunctioncall,"*",[]).
transition(reptvariableorfunctioncall,"]",[]).
transition(fparams,id,[id,":",type,reptfparams3,reptfparams4]).
transition(fparams,")",[]).
transition(reptfparams3,")",[]).
transition(reptfparams3,",",[]).
transition(reptfparams3,"[",[arraysize,reptfparams3]).
transition(reptfparams4,")",[]).
transition(reptfparams4,",",[fparamstail,reptfparams4]).
transition(reptfparams4,",",[fparamstail,reptfparams4]).
transition(reptfparamstail4,")",[]).
transition(reptfparamstail4,",",[]).
transition(reptfparamstail4,"[",[arraysize,reptfparamstail4]).
transition(fparamstail,",",[",",id,":",type,reptfparamstail4]).
transition(funcbody,"{",["{",reptfuncbody,"}"]).
transition(reptfuncbody,id,[vardeclorstat,reptfuncbody]).
transition(reptfuncbody,"let",[vardeclorstat,reptfuncbody]).
transition(reptfuncbody,"while",[vardeclorstat,reptfuncbody]).
transition(reptfuncbody,"read",[vardeclorstat,reptfuncbody]).
transition(reptfuncbody,"write",[vardeclorstat,reptfuncbody]).
transition(reptfuncbody,"if",[vardeclorstat,reptfuncbody]).
transition(reptfuncbody,"}",[]).
transition(funcdecl,"func",[funchead,";"]).
transition(funcdef,"func",[funchead,funcbody]).
transition(funchead,"func",["func",id,"(",fparams,")","->",returntype]).
transition(idnest,id,[id,idnest2]).
transition(idnest,id,[".",id,idnest2]).
transition(idnest2,"(",["(",aparams,")","."]).
transition(idnest2,")",[reptidnest]).
transition(idnest2,";",[reptidnest]).
transition(idnest2,".",[reptidnest]).
transition(idnest2,"+",[reptidnest]).
transition(idnest2,"-",[reptidnest]).
transition(idnest2,">=",[reptidnest]).
transition(idnest2,"<=",[reptidnest]).
transition(idnest2,">",[reptidnest]).
transition(idnest2,"<",[reptidnest]).
transition(idnest2,"!=",[reptidnest]).
transition(idnest2,"==",[reptidnest]).
transition(idnest2,",",[reptidnest]).
transition(idnest2,"&",[reptidnest]).
transition(idnest2,"/",[reptidnest]).
transition(idnest2,"*",[reptidnest]).
transition(idnest2,"|",[reptidnest]).
transition(idnest2,"[",[reptidnest]).
transition(idnest2,"]",[reptidnest]).
transition(reptidnest,"[",[indice,reptidnest]).
transition(reptidnest,".",[]).
transition(reptidnest,")",[]).
transition(reptidnest,";",[]).
transition(reptidnest,"+",[]).
transition(reptidnest,"-",[]).
transition(reptidnest,">=",[]).
transition(reptidnest,"<=",[]).
transition(reptidnest,">",[]).
transition(reptidnest,"<",[]).
transition(reptidnest,"!=",[]).
transition(reptidnest,"==",[]).
transition(reptidnest,",",[]).
transition(reptidnest,"&",[]).
transition(reptidnest,"/",[]).
transition(reptidnest,"*",[]).
transition(reptidnest,"]",[]).
transition(reptidnest,"|",[]).
transition(impldef,"impl",["impl",id,"{",reptfuncdef,"}"]).
transition(reptfuncdef,"func",[funcdef,reptfuncdef]).
transition(reptfuncdef,"}",[]).
transition(indice,"[",["[",arithexpr,"]"]).
transition(memberdecl,"let",[vardecl]).
transition(memberdecl,"func",[funcdecl]).
transition(multop,"&",["&"]).
transition(multop,"/",["/"]).
transition(multop,"*",["*"]).
transition(structinherits,"{",[]).
transition(structinherits,"inherits",["inherits",id,reptstructinherits]).
transition(reptstructinherits,",",[",",id,reptstructinherits]).
transition(reptstructinherits,"{",[]).
transition(relexpr,id,[arithexpr,relop,arithexpr]).
transition(relexpr,"(",[arithexpr,relop,arithexpr]).
transition(relexpr,"+",[arithexpr,relop,arithexpr]).
transition(relexpr,"-",[arithexpr,relop,arithexpr]).
transition(relexpr,"!",[arithexpr,relop,arithexpr]).
transition(relexpr,intnum,[arithexpr,relop,arithexpr]).
transition(relexpr,floatnum,[arithexpr,relop,arithexpr]).
transition(relop,">=",[">="]).
transition(relop,"<=",["<="]).
transition(relop,">",[">"]).
transition(relop,"<",["<"]).
transition(relop,"!=",["!="]).
transition(relop,"==",["=="]).
transition(returntype,id,[type]).
transition(returntype,"void",["void"]).
transition(returntype,"integer",[type]).
transition(returntype,"float",[type]).
transition(rightrecarithexpr,";",[]).
transition(rightrecarithexpr,")",[]).
transition(rightrecarithexpr,"+",[addop,term,rightrecarithexpr]).
transition(rightrecarithexpr,"-",[addop,term,rightrecarithexpr]).
transition(rightrecarithexpr,">=",[]).
transition(rightrecarithexpr,"<=",[]).
transition(rightrecarithexpr,">",[]).
transition(rightrecarithexpr,"<",[]).
transition(rightrecarithexpr,"!=",[]).
transition(rightrecarithexpr,"==",[]).
transition(rightrecarithexpr,",",[]).
transition(rightrecarithexpr,"]",[]).
transition(rightrecarithexpr,"|",[addop,term,rightrecarithexpr]).
transition(rightrecterm,")",[]).
transition(rightrecterm,"+",[]).
transition(rightrecterm,"-",[]).
transition(rightrecterm,">=",[]).
transition(rightrecterm,"<=",[]).
transition(rightrecterm,">",[]).
transition(rightrecterm,"<",[]).
transition(rightrecterm,"!=",[]).
transition(rightrecterm,"==",[]).
transition(rightrecterm,",",[]).
transition(rightrecterm,"&",[multop,factor,rightrecterm]).
transition(rightrecterm,"/",[multop,factor,rightrecterm]).
transition(rightrecterm,"*",[multop,factor,rightrecterm]).
transition(rightrecterm,"]",[]).
transition(rightrecterm,"|",[]).
transition(rightrecterm,";",[]).
transition(sign,"-",["-"]).
transition(sign,"+",["+"]).
transition(statblock,id,[statement]).
transition(statblock,";",[]).
transition(statblock,"{",["{",statement,"}"]).
transition(statblock,"return",[statement]).
transition(statblock,"read",[statement]).
transition(statblock,"write",[statement]).
transition(statblock,"while",[statement]).
transition(statblock,"if",[statement]).
transition(statblock,"else",[]).
transition(statement,"return",["return","(",expr,")",";"]).
transition(statement,"write",["write","(",expr,")",";"]).
transition(statement,"read",["read","(",variable,")",";"]).
transition(statement,"while",["while","(",relexpr,")",statblock,";"]).
transition(statement,"if",["if","(",relexpr,")","then",statblock,"else",statblock,";"]).
transition(statement,id,[id,statementidnest,";"]).
transition(statementidnest,"(",["(",aparams,")",statementidnest2]).
transition(statementidnest,".",[".",id,statementidnest]).
transition(statementidnest,"[",[indice,reptidnest,statementidnest3]).
transition(statementidnest,"=",[assignop,expr]).
transition(statementidnest2,";",[]).
transition(statementidnest2,".",[".",id,statementidnest]).
transition(statementidnest3,".",[".",id,statementidnest]).
transition(statementidnest3,"=",[assignop,expr]).
transition(statement2,id,[id,indice,assignop,expr]).
transition(statement2,"(",["(",aparams,")"]).
transition(structdecl,"struct",["struct",id,structinherits,"{",reptstructdecl,"}",";"]).
transition(reptstructdecl,"public",[visibility,memberdecl,reptstructdecl]).
transition(reptstructdecl,"private",[visibility,memberdecl,reptstructdecl]).
transition(reptstructdecl,"}",[]).
transition(structorimplorfunc,"struct",[structdecl]).
transition(structorimplorfunc,"impl",[impldef]).
transition(structorimplorfunc,"func",[funcdef]).
transition(term,id,[factor,rightrecterm]).
transition(term,"(",[factor,rightrecterm]).
transition(term,"-",[factor,rightrecterm]).
transition(term,"+",[factor,rightrecterm]).
transition(term,"!",[factor,rightrecterm]).
transition(term,floatnum,[factor,rightrecterm]).
transition(term,intnum,[factor,rightrecterm]).
transition(type,"float",["float"]).
transition(type,"integer",["integer"]).
transition(type,"id",["id"]).
transition(type,id,[id]).
transition(vardecl,"let",["let",id,":",type,reptvardecl,";"]).
transition(reptvardecl,"[",[arraysize,reptvardecl]).
transition(reptvardecl,";",[]).
transition(vardeclorstat,id,[statement]).
transition(vardeclorstat,"let",[vardecl]).
transition(vardeclorstat,"return",[statement]).
transition(vardeclorstat,"read",[statement]).
transition(vardeclorstat,"write",[statement]).
transition(vardeclorstat,"while",[statement]).
transition(vardeclorstat,"if",[statement]).
transition(variable,id,[id,variable2]).
transition(variable2,"[",[reptidnest,reptvariable]).
transition(variable2,".",[reptidnest,reptvariable]).
transition(variable2,"(",["(",aparams,")",varidnest]).
transition(variable2,")",[reptidnest,reptvariable]).
transition(reptvariable,".",[varidnest,reptvariable]).
transition(reptvariable,")",[]).
transition(varidnest,".",[".",id,reptidnest]).
transition(varidnest2,"[",[reptidnest]).
transition(varidnest2,".",[reptidnest]).
transition(varidnest2,"(",["(",aparams,")",varidnest]).
transition(varidnest2,")",[reptidnest]).
transition(visibility,"public",["public"]).
transition(visibility,"private",["private"]).


% ?- nonterminal(start,_,_).
%@    true.


%nonterminal(Nonterm,First,Follow).
nonterminal(start,["struct","impl","func"],[]).
nonterminal(reptprogram,["struct","impl","func"],[]).
nonterminal(arraypamstail,[","],[")"]).
nonterminal(arraysizep,[intnum],["]"]).
nonterminal(expr2,["==","!=","<",">","<=",">="],[";",",",")"]).
nonterminal(factor2,["(","["],
	    [";","*","/","&",".","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(reptvariableorfunctioncall,["."],
	     [";","*","/","&","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(reptfparams3,["["],[")",","]).
nonterminal(reptfparams4,[","],[")"]).
nonterminal(fparamstail,[","],[")",","]).
nonterminal(reptfparamstail4,["["],[")",","]).
nonterminal(funcbody,["{"],["}","struct","impl","func"]).
nonterminal(reptfuncbody,["let","if","while","read","write","return",id],["}"]).
nonterminal(funchead,["func"],[")"]).
nonterminal(fparams,[id],[")"]).
nonterminal(idnest,["."],
	    [";","*","/","&","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(idnest2,["(","["],
	    [";","*","/","&","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(funcdecl,["func"],["public","private","}"]).
nonterminal(funcdecl,["func"],["}"]).
nonterminal(reptstructinherits,[","],["{"]).
nonterminal(arithexpr,[intnum,floatnum,id,"(","!","+","-"],
	    [";","}","==","!=","<",">","<=",">=",",",")"]).
nonterminal(relop,["==","!=","<",">","<=",">="],[intnum,floatnum,id,"(","!","+","-"]).
nonterminal(returntype,["void","integer","float",id],[";","}"]).
nonterminal(addop,["+","=","|"],[intnum,floatnum,id,"(","!","+","-"]).
nonterminal(rightrecarithexpr,["+","-","|"],[";","}","==","!=","<",">","<=",">=",",",")"]).
nonterminal(multop,["*","/","&"],[intnum,floatnum,id,"(","!","+","-"]).
nonterminal(sign,["+","-"],[intnum,floatnum,id,"(","!","+","-"]).
nonterminal(relexpr,[intnum,floatnum,id,"(","!","+","-"],[")"]).
nonterminal(statblock,["{","if","while","read","write","return",id],["else",";"]).
nonterminal(statementidnest2,["."],[";"]).
nonterminal(statementidnest3,[".","="],[";"]).
nonterminal(assignop,["="],[intnum,floatnum,"(","!",id,"+","-"]).
nonterminal(expr,[intnum,floatnum,"(","!",id,"+","-"],[";",",",")"]).
nonterminal(statementidnest,[".","(","[","="],[";"]).
nonterminal(indice,["["],
	    [";","*","/","&","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(aparams,[intnum,floatnum,"(","!",id,"+","-"],[")"]).
nonterminal(structinherits,["inherits"],["{"]).
nonterminal(memberdecl,["let","func"],["public","private","}"]).
nonterminal(structorimplorfunc,["struct","impl","func"],["struct","impl","func"]).
nonterminal(reptstructdecl,["struct","impl","func"],["}"]).
nonterminal(impldef,["impl"],["struct","impl","func"]).
nonterminal(funcdef,["func"],["}"]).
nonterminal(structdecl,["struct"],["struct","impl","func"]).
nonterminal(term,[intnum,floatnum,id,"(","!","+","-"],
	    [";","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(factor,[intnum,floatnum,id,"(","!","+","-"],
	    [";","*","/","&","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(rightrecterm,["*","/","&"],
	    [";","[","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(type,["integer","float","id",id],[";","{","["]).
nonterminal(arraysize,["["],[")",",",";"]).
nonterminal(vardecl,["let"],["}"]).
nonterminal(reptvardecl,["["],[";"]).
nonterminal(vardeclorstat,["let","if","while","read","write","return",id],
	    ["public","private","}"]).
nonterminal(statement,["if","while","read","write","return",id],
	    ["else",";","}","let",id,"if","while","read","write","write"]).
nonterminal(variable,["."],[")"]).
nonterminal(variable2,["(","[","."],[")"]).
nonterminal(varidnest2,["(","["],[".",")"]).
nonterminal(varidnest,["."],[".",")"]).
nonterminal(reptidnest,["["],
	    [".",";","]","==","!=","<",">","<=",">=","+","-","|",",",")"]).
nonterminal(visibility,["public","private"],["let","func"]).


terminal(id).
terminal(intnum).
terminal(floatnum).
terminal("void").
terminal("float").
terminal("integer").
terminal(X):-token(X,_).





% ?- nonterminal(statblock,X,Y),id("ap").
%@    X = ["{","if","while","read","write","return","a"], Y = ["}"]
%@ ;  ...


%table_driven_parse(+Tokenlist,Stack,LineNumber).
% stack starts where on the stack you wish, [start,'$'] for start
%table_driven_parse([],[],_Ln,_O).
table_driven_parse(['$'],['$'],_Ln,_O).
table_driven_parse(["\n"|T],[Top|T2],Ln0,O):-
    Ln #= Ln0 + 1,
    table_driven_parse(T,[Top|T2],Ln,O).
table_driven_parse([H|T],[Top|T2],Ln,O):-
    dynamic_terminal(Top,H),
    %write(11),write(':'),write(Ln),write(H),write([Top|T2]),nl,
    table_driven_parse(T,T2,Ln,O).
table_driven_parse([H|T],[Top|T2],Ln,O):-
    terminal(Top),
    H = Top,
    %write(12),write(':'),write(Ln),write(H),write([Top|T2]),nl,
    table_driven_parse(T,T2,Ln,O).
table_driven_parse([H|T],[Top|T2],Ln,O):-
    transition(Top,H,Next),
    %write(22),write(':'),write(Ln),write(H),write(Next),write([Top|T2]),nl,
    append(Next,T2,Stack),
%    write(23),write(H),write(Top),write(Stack),nl,
    table_driven_parse([H|T],Stack,Ln,O).
table_driven_parse([H|T],[Top|T2],Ln,O):-
    nonterminal(Top,_,_),
    dynamic_terminal(Type,H),
    transition(Top,Type,Next),
    %write(22),write(':'),write(Ln),write(H),write(Next),write([Top|T2]),nl,
    append(Next,T2,Stack),
%    write(23),write(Stack),nl,
    table_driven_parse([H|T],Stack,Ln,O).
table_driven_parse([H|T],[H2|T2],Ln0,O):-
    write(O,'error line:'),write(O,Ln0),write(O,' '),
    write(O,H),write(O,[H2|T2]),write(O,'\n'),
    %write(H),nl,write([H2|T2]),nl,
    skip_errors([H|T],[H2|T2],Tokens,Stack,Ln0,Ln),!,
    write(O,['Resumed parse',Ln]),write(O,'\n'),
    table_driven_parse(Tokens,Stack,Ln,O).
table_driven_parse(['$'],NTs,Ln,O):-
    write(O,'Eof: NonTerminals left'),write(O,NTs),
    table_driven_parse(['$'],['$'],Ln,O).
table_driven_parse([],NTs,Ln,O):-
    write(O,'Eof: NonTerminals left'),write(O,NTs),
    table_driven_parse(['$'],['$'],Ln,O).


% skip_errors(+Tokenlist,+Stack,-RestTokens,-RestStack,+Linenumber,Linenumber-).
skip_errors([],L,['$'],L,Ln,Ln):-!.
skip_errors(['$'],L,['$'],L,Ln,Ln):-!.
skip_errors([H1|T1],[Top|T2],[H1|T1],[Top|T2],Ln,Ln):-
    nonterminal(Top,_,Follow),
    member(H1,Follow).
% pops when the followset is null
skip_errors([H1|T1],[Top|T2],L1,L2,Ln0,Ln):-
    nonterminal(Top,_,[]),
    %write(['Popped Stack',Top]),nl,
    skip_errors([H1|T1],T2,L1,L2,Ln0,Ln).
% pops stack when its a terminal that doesn't match
skip_errors([H1|T1],[Top|T2],L1,L2,Ln0,Ln):-
    terminal(Top),
    %write(['Popped Stack',Top]),nl,
    skip_errors([H1|T1],T2,L1,L2,Ln0,Ln).
skip_errors([H1|T1],[Top|T2],L1,L2,Ln0,Ln):-
    %write(['Popped token',H1]),nl,
    (H1 = "\n"
    -> Ln1 #= Ln0 + 1
    ; Ln1 = Ln0),
    skip_errors(T1,[Top|T2],L1,L2,Ln1,Ln).


dynamic_terminal(id,I):-id(I).
dynamic_terminal(floatnum,I):-float_(I).
dynamic_terminal(intnum,I):-integer_(I).

% ?- dynamic_terminal(T,"main").
%@    T = id
%@ ;  false.


test_parser(Filein):-
    split_delimiter(Filein,N,_R,'.'),
    write(N),nl,nl,
    append(N,"_parse_errors.txt",ErrorFile0),
    atom_chars(ErrorFile,ErrorFile0),
    open(Filein,read,S1,[]),
    open(ErrorFile,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    fix_floats(Lst2,Lst3),
    phrase(remove_comment_blocks(Lst4),Lst3),
    remove_inline(Lst4,Lst5),
    append(Lst5,['$'],Lst6),
    table_driven_parse(Lst6,[start,'$'],0,S2),!,
    close(S1),
    close(S2).

    
/*



?- open("filetest.txt",write,S,[]),table_driven_parse(Tokenlist,Stack,0,S).




?- test_parser("polynomial.src").
%@ "polynomial"
%@ 
%@ 
%@ caught: error('$interrupt_thrown',repl)



?- test_parser("bubblesort.src").
%@ "bubblesort"
%@ 
%@    true.



% ?- nonterminal(reptprogram,X,Y).
%@    X = ["struct","impl","func"], Y = [].


?- test_parser("exprs.src").
%@ "exprs"
%@ 
%@    true.


?- test_parser("hello.src").
%@ "hello"
%@ 
%@    true.


% ?- transition(structdecl,X,Y).
%@    X = "struct", Y = ["struct",id,optstructdecl2,"{",visibility,memberdecl,"}",";"].

/*

test case

error recovery ?
- for every branch block have a final test case thats just a generic sequence and results in printing that there was a semantic error.




*/
/*
In Progress
*/
get_table([Td1,Td2,Td3]) :-
    http_open("https://smlweb.cpsc.ucalgary.ca/vital-stats.php?grammar=START+-%3E+STRUCTORIMPLORFUNC+.%0D%0A%0D%0AADDOP+-%3E+plus+%0D%0A++++++++%7Cminus+%0D%0A++++++++%7Cor+.%0D%0A%0D%0AAPARAMS+-%3E+EXPR+APARAMSTAIL+%0D%0A++++++++++%7C.%0D%0A%0D%0AAPARAMSTAIL+-%3E+comma+EXPR+.%0D%0A%0D%0AARITHEXPR+-%3E+TERM+RIGHTRECARITHEXPR+.%0D%0A%0D%0AARRAYSIZE+-%3E+lsqbr+ARRAYSIZEP+rsqbr+.%0D%0A%0D%0AARRAYSIZEP+-%3E+intnum+%0D%0A+++++++++++++%7C.%0D%0A%0D%0AASSIGNOP+-%3E+equal+.%0D%0A%0D%0AEXPR+-%3E++ARITHEXPR+EXPR2.%0D%0A%0D%0AEXPR2+-%3E+RELOP+ARITHEXPR%0D%0A++++++%7C.%0D%0A%0D%0AFACTOR+-%3E+IDNEST+FIDNEST+%0D%0A+++++++++%7Cintlit+%0D%0A+++++++++%7Cfloatlit+%0D%0A+++++++++%7Clpar+ARITHEXPR+rpar+%0D%0A+++++++++%7Cnot+FACTOR+%0D%0A+++++++++%7CSIGN+FACTOR+.%0D%0A%0D%0AFIDNEST+-%3E+id+Fid_1+.%0D%0A%0D%0AFid_1+-%3E+INDICE+%0D%0A++++++++%7Clpar+APARAMS+rpar+.%0D%0A%0D%0AFPARAMS+-%3E+id+colon+TYPE+ARRAYSIZE+FPARAMSTAIL+%0D%0A++++++++++%7C.%0D%0A%0D%0AFPARAMSTAIL+-%3E+comma+id+colon+TYPE+ARRAYSIZE+.%0D%0A%0D%0AFUNCBODY+-%3E+lcurbr+VARDECLORSTAT+rcurbr+.%0D%0A%0D%0AFUNCDECL+-%3E+FUNCHEAD+semi+.%0D%0A%0D%0AFUNCDEF+-%3E+FUNCHEAD+FUNCBODY+.%0D%0A%0D%0AFUNCHEAD+-%3E+func+id+lpar+FPARAMS+rpar+minusarrow+RETURNTYPE+.%0D%0A%0D%0AIDNEST+-%3E++id+Fid+.%0D%0A%0D%0AFid+-%3E+INDICE+dot+%0D%0A++++++%7Clpar+APARAMS+rpar+dot+.%0D%0A%0D%0AIMPLDEF+-%3E+impl+id+lcurbr+FUNCDEF+rcurbr+.%0D%0A%0D%0AINDICE+-%3E+lsqbr+ARITHEXPR+rsqbr+.%0D%0A%0D%0AMEMBERDECL+-%3E+FUNCDECL+%0D%0A+++++++++++++%7CVARDECL+.%0D%0A%0D%0AMULTOP+-%3E+mult+%0D%0A+++++++++%7Cdiv+%0D%0A+++++++++%7Cand+.%0D%0A%0D%0AOPTSTRUCTDECL2+-%3E+inherits+id+comma+id+%0D%0A+++++++++++++++++%7C.%0D%0A%0D%0A%0D%0ARELEXPR+-%3E+ARITHEXPR+RELOP+ARITHEXPR+.%0D%0A%0D%0ARELOP+-%3E+eq+%0D%0A++++++++%7Cneq+%0D%0A++++++++%7Clt+%0D%0A++++++++%7Cgt+%0D%0A++++++++%7Cleq+%0D%0A++++++++%7Cgeq+.%0D%0A%0D%0ARETURNTYPE+-%3E+TYPE+%0D%0A+++++++++++++%7Cvoid+.%0D%0A%0D%0ARIGHTRECARITHEXPR+-%3E+%0D%0A++++++++++++++++++++%7CADDOP+TERM+RIGHTRECARITHEXPR+.%0D%0A%0D%0ARIGHTRECTERM+-%3E+%0D%0A+++++++++++++++%7CMULTOP+FACTOR+RIGHTRECTERM+.%0D%0A%0D%0A%0D%0ASIGN+-%3E+plus+%0D%0A+++++++%7Cminus+.%0D%0A%0D%0ASTATBLOCK+-%3E+lcurbr+STATEMENT+rcurbr+%0D%0A++++++++++++%7CSTATEMENT+%0D%0A++++++++++++%7C.%0D%0A%0D%0ASTATEMENT+-%3E+IDNEST+STATEMENT2+semi%0D%0A++++++++++++%7Cif+lpar+RELEXPR+rpar+then+STATBLOCK+else+STATBLOCK+semi+%0D%0A++++++++++++%7Cwhile+lpar+RELEXPR+rpar+STATBLOCK+semi+%0D%0A++++++++++++%7Cread+lpar+VARIABLE+rpar+semi+%0D%0A++++++++++++%7Cwrite+lpar+EXPR+rpar+semi+%0D%0A++++++++++++%7Creturn+lpar+EXPR+rpar+semi.%0D%0A%0D%0A%0D%0ASTATEMENT2+-%3E++id+INDICE+ASSIGNOP+EXPR+%0D%0A%09+++++%7C+lpar+APARAMS+rpar+.%0D%0A%0D%0A%0D%0A%0D%0A%0D%0ASTRUCTDECL+-%3E+struct+id+OPTSTRUCTDECL2+lcurbr+VISIBILITY+MEMBERDECL+rcurbr+semi+.%0D%0A%0D%0ASTRUCTORIMPLORFUNC+-%3E+STRUCTDECL+%0D%0A+++++++++++++++++++++%7CIMPLDEF+%0D%0A+++++++++++++++++++++%7CFUNCDEF+.%0D%0A%0D%0ATERM+-%3E+FACTOR+RIGHTRECTERM+.%0D%0A%0D%0ATYPE+-%3E+integer+%0D%0A+++++++%7Cfloat+%0D%0A+++++++%7Cid+.%0D%0A%0D%0AVARDECL+-%3E+let+id+colon+TYPE+ARRAYSIZE+semi+.%0D%0A%0D%0AVARDECLORSTAT+-%3E+VARDECL+%0D%0A++++++++++++++++%7CSTATEMENT+.%0D%0A%0D%0AVARIABLE+-%3E++IDNEST+id+INDICE.%0D%0A%0D%0AVISIBILITY+-%3E+public+%0D%0A+++++++++++++%7Cprivate+.%0D%0A",S,[]),
    load_html(stream(S),DOM,[]),
    xpath(DOM, //table(@class(stats)), Table),
    xpath(Table, //tr, TRs),
    xpath(TRs, td(1), Td1),
    xpath(TRs, td(2), Td2),
    xpath(TRs, td(3), Td3).

generate_firstset([Nonterms,Firstsets,Followsets]):-
    findall(X,get_firsts([X,_Y,_Z]),X1),
    findall(Y,get_firsts([_X,Y,_Z]),Y1),
    findall(Z,get_firsts([_X,_Y,Z]),Z1),
    write(X1),nl,
    write(Y1),nl,
    write(Z1),nl,
    get_terms(X1,Nonterms),
    get_terms(Y1,Firstsets),
    get_terms(Z1,Followsets).



get_terms([],[]).
get_terms([H|T1],[F|T2]):-
    xpath(H, nonterm(text), F),
    get_terms(T1,T2).

translate("lcurbr","{").
translate("rcurbr","}").


write_list([]).
write_list([H|T]):-
    xpath(H, //nonterm,L),
    write(L),nl,
    write_list(T).


% ?- findall([X,Y,Z],get_firsts([X,Y,Z]),Sets),write_list(Sets).


% ?- get_firsts([X,Y,Z]),write_list([X,Y,Z]).



% ?- generate_firstset(Fs).



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

structdecl_ast(node(struct,[ID,Opt|ReptStruct])) --> ["struct"],id(ID),
						  structinherits_ast(Opt),
					     ["{"], reptstructdecl_ast(ReptStruct),
					     ["}"], [";"].

reptstructdecl_ast([node(struct_decl,[Vis,Mem])|N]) --> visibility_ast(Vis),
				       memberdecl_ast(Mem),
				       reptstructdecl_ast(N).   
reptstructdecl_ast([]) --> [].

reptstructinherits_ast(node(inherits,[ID|N])) --> [","], id(ID), reptstructinherits_ast(N).   
reptstructinherits_ast([]) --> [].

structinherits_ast(node(inherits,[ID|N])) --> ["inherits"], id(ID),reptstructinherits_ast(N).  
structinherits_ast([]) --> [].

% Impl definition

impldef_ast(node(impl_def,[ID|Impl])) --> ["impl"],id(ID),
					  ["{"], reptimpldef3_ast(Impl), ["}"].  

reptimpldef3_ast([F|N]) --> funcdef_ast(F), reptimpldef3_ast(N).   
reptimpldef3_ast([]) --> [].

% Function definitions
funcdecl_ast(node(func_decl,[F])) --> funchead_ast(F) ,[";"].  

funcdef_ast(node(func_def,[Head,Body])) --> funchead_ast(Head), funcbody_ast(Body).  

funchead_ast(node(func_head,[ID,Params,ReturnType])) --> ["func"], id(ID),
			       ["("], fparams_ast(Params), [")"],
			       ["->"], returntype_ast(ReturnType).  

funcbody_ast(node(func_body,Body)) --> ["{"], reptfuncbody1_ast(Body), ["}"].

reptfuncbody1_ast([node(func_body,[B])|N]) --> vardeclorstat_ast(B), reptfuncbody1_ast(N).
reptfuncbody1_ast([]) --> [].


% any Expression
% often of form of 18*4 or a / (h[3] + 2)
expr_ast(node(expr,[Expr,Rhs])) --> arithexpr_ast(Expr), expr2_ast(Rhs). 

expr2_ast(node(relop,[Op,Arith])) --> relop_ast(Op), arithexpr_ast(Arith).  
expr2_ast([]) --> [].


% arithmetic expressions
arithexpr_ast(node(arith_expr,[Term|Rhs])) --> term_ast(Term), rightrecarithexpr_ast(Rhs).


rightrecarithexpr_ast([node(addop,[Op,Term|N])]) --> addop_ast(Op), term_ast(Term), rightrecarithexpr_ast(N).
rightrecarithexpr_ast([]) -->  [].

term_ast(node(term,[Factor,Next])) --> factor_ast(Factor), rightrecterm_ast(Next).


rightrecterm_ast(node(term,[node(multop,[Op]),Factor,T])) --> multop_ast(Op), factor_ast(Factor), rightrecterm_ast(T).   
rightrecterm_ast([]) -->  [].


factor_ast(node(factor,[ID,F2,RVOFC])) --> id(ID), factor2_ast(F2), reptvariableorfunctioncall_ast(RVOFC).  
factor_ast(node(factor,[terminal(intnum,[I])])) --> [I],{integer_(I)}.
factor_ast(node(factor,[terminal(floatnum,[F])])) --> [F],{float_(F)}.
factor_ast(node(factor,[F])) --> ["("], arithexpr_ast(F), [")"].
factor_ast(node(factor,[node(not,[F])])) --> ["!"], factor_ast(F).
factor_ast(node(factor,[node(sign,[S]),F])) --> sign_ast(S), factor_ast(F).
%factor_ast(node(factor,[terminal(floatnum,[F])])) --> seq(F).

%factor_ast(node(factor,[terminal(floatnum,[F])])) --> I1,["."],I2,{integer(I1),integer(I2)}.
%factor_ast(node(factor,[terminal(floatnum,[F])])) --> I1,{integer(I1)},["."],I2,{integer(I2)}.

factor2_ast(F2) --> ["("], aparams_ast(F2), [")"].
factor2_ast(F2) --> reptidnest1_ast(F2).  

fparams_ast(node(fparam,[ID,Type|Rept4])) --> id(ID), [":"],
						  type_ast(Type),
						  reptfparams4_ast(Rept4).
fparams_ast(node(fparam,[ID,Type,Rept3|Rept4])) --> id(ID), [":"],
							type_ast(Type),
							reptfparams3_ast(Rept3),
							reptfparams4_ast(Rept4).
fparams_ast([]) --> [].

fparamstail_ast(node(fparam,[ID,Type|ReptTail])) --> [","], id(ID), [":"], type_ast(Type),
							 reptfparamstail4_ast(ReptTail).

reptfparams3_ast([Size|Rept3]) --> arraysize_ast(Size), reptfparams3_ast(Rept3).  
reptfparams3_ast([]) --> [].

reptfparams4_ast([Tail|Param]) --> fparamstail_ast(Tail), reptfparams4_ast(Param).
reptfparams4_ast([]) --> [].

reptfparamstail4_ast([Size|ParamTail]) --> arraysize_ast(Size), reptfparamstail4_ast(ParamTail).  
reptfparamstail4_ast([]) --> [].


arraysize_ast(Size) --> ["["], arraysize2_ast(Size).  

arraysize2_ast(terminal(arr,[Size])) --> [Size],{integer_(Size)}, ["]"]. 
arraysize2_ast(terminal(arr,[])) --> ["]"].


reptaparams1_ast(node(aparam,[P,N])) --> aparamstail_ast(P), reptaparams1_ast(N).  
reptaparams1_ast([]) --> [].


% array parameters
aparams_ast(node(aparam,[P,T])) --> expr_ast(P), reptaparams1_ast(T).
aparams_ast([]) --> [].

% array paramters tail
aparamstail_ast(X) --> [","], expr_ast(X).  



statement_ast(node(if,[Expr,StatblockIf,StatblockElse])) --> ["if"], ["("], relexpr_ast(Expr), [")"], ["then"],
						      statblock_ast(StatblockIf), ["else"],
						      statblock_ast(StatblockElse), [";"].  
statement_ast(node(while,[Expr,Statblock])) --> ["while"], ["("], relexpr_ast(Expr), [")"],
					 statblock_ast(Statblock), [";"].  
statement_ast(node(read,[Variable])) --> ["read"], ["("], variable_ast(Variable), [")"], [";"].  
statement_ast(node(write,[Expr])) --> ["write"], ["("], expr_ast(Expr), [")"], [";"].  
statement_ast(node(return,[Expr])) --> ["return"], ["("], expr_ast(Expr), [")"], [";"].  
statement_ast(node(id_statement,[ID,Idnest])) --> id(ID), statementidnest_ast(Idnest), [";"]. 


statementidnest_ast(node(dot,[ID,Idnest])) --> ["."], id(ID), statementidnest_ast(Idnest).
statementidnest_ast(node(func_call,[Params,Idnest])) --> ["("], aparams_ast(Params), [")"], statementidnest2_ast(Idnest).
statementidnest_ast(node(indice,[Indice,Idnest,StatementIdNest])) --> indice_ast(Indice), reptidnest1_ast(Idnest), statementidnest3(StatementIdNest).
statementidnest_ast(node(assign,[Expr])) --> assignop_ast, expr_ast(Expr).


statementidnest2_ast(node(nested_id,[ID,Nest])) --> ["."], id(ID), statementidnest_ast(Nest).
statementidnest2_ast([]) --> [].

statementidnest3(node(assign,[Expr])) --> assignop_ast, expr_ast(Expr).
statementidnest3(node(dot,[ID,IDnest])) --> ["."], id(ID), statementidnest_ast(IDnest).  

reptidnest1_ast(node(idnest,[Indice,N])) --> indice_ast(Indice), reptidnest1_ast(N).  
reptidnest1_ast([]) --> [].

reptvariableorfunctioncall_ast(node(id_nest,[node(id,[ID])|N])) -->  idnest_ast(ID), reptvariableorfunctioncall_ast(N).  
reptvariableorfunctioncall_ast([]) -->  [].

idnest_ast(node(dot,[node(idnest,[ID,N])])) --> ["."], id(ID), idnest2_ast(N).  

idnest2_ast(node(func_call,[Param])) --> ["("], aparams_ast(Param), [")"].  
idnest2_ast(N) --> reptidnest1_ast(N).  

indice_ast(node(indice,[Indice])) --> ["["], arithexpr_ast(Indice), ["]"].  

memberdecl_ast(node(member,[Fun])) --> funcdecl_ast(Fun).  
memberdecl_ast(Var) --> vardecl_ast(Var).  

relexpr_ast(node(relational_expr,[Arith,Rel])) --> arithexpr_ast(Arith),
						    rightrelexpr(Rel).
rightrelexpr(node(relop,[Rel,Arith])) --> relop_ast(Rel),
				   arithexpr_ast(Arith).

reptvardecl4_ast([Arr,N]) --> arraysize_ast(Arr), reptvardecl4_ast(N).   
reptvardecl4_ast([]) --> [].

		 
statblock_ast(node(statblock,[S])) --> ["{"], reptstatblock1_ast(S), ["}"].   
statblock_ast(node(statblock,[S])) --> statement_ast(S).   
statblock_ast([]) -->  [].

reptstatblock1_ast(node(statblock,[S,N])) --> statement_ast(S), reptstatblock1_ast(N).   
reptstatblock1_ast([]) --> [].

vardecl_ast(node(var,[ID,Type|N])) --> ["let"], id(ID), [":"], type_ast(Type), reptvardecl4_ast(N), [";"].   

vardeclorstat_ast(V) --> vardecl_ast(V).   
vardeclorstat_ast(S) --> statement_ast(S).   

variable_ast(node(var,[ID,N])) -->  id(ID), variable2_ast(N).  

variable2_ast(node(var,[Idnest,N])) -->  reptidnest1_ast(Idnest), reptvariable_ast(N).    
variable2_ast(node(var,[Param,Idnest])) -->  ["("], aparams_ast(Param), [")"], varidnest_ast(Idnest).

reptvariable_ast(node(var,[V,N])) -->  varidnest_ast(V), reptvariable_ast(N).   
reptvariable_ast([]) --> [].

varidnest_ast(node(var,[ID,N])) --> ["."], id(ID), varidnest2_ast(N).
varidnest2_ast(nod(nest,[P,Idnest])) --> ["("], aparams_ast(P), [")"], varidnest(Idnest).  
varidnest2_ast(Idnest) --> reptidnest1_ast(Idnest).


relop_ast(terminal(relop,["=="])) --> ["=="].
relop_ast(terminal(relop,["!="])) -->  ["!="].
relop_ast(terminal(relop,["<"])) --> ["<"].
relop_ast(terminal(relop,[">"])) --> [">"].
relop_ast(terminal(relop,["<="])) -->  ["<="].
relop_ast(terminal(relop,[">="])) -->  [">="].

		 
returntype_ast(T) --> type_ast(T).   
returntype_ast(terminal(type,["void"])) --> ["void"].

sign_ast(terminal(sign,["+"])) --> ["+"].
sign_ast(terminal(sign,["-"])) --> ["-"].

assignop_ast --> ["="].
		       
type_ast(terminal(type,["integer"])) --> ["integer"].
type_ast(terminal(type,["float"])) --> ["float"].
type_ast(terminal(type,["id"])) --> ["id"].
type_ast(terminal(type,[ID])) --> [ID],{id(ID)}.


% addition operators 
addop_ast(terminal(addop,["+"])) --> ["+"].
addop_ast(terminal(addop,["-"])) --> ["-"].
addop_ast(terminal(addop,["|"])) --> ["|"].

% multiplication operators
multop_ast(terminal(multop,["*"])) --> ["*"].
multop_ast(terminal(multop,["/"])) --> ["/"].
multop_ast(terminal(multop,["&"])) --> ["&"].

visibility_ast(terminal(visibility,["public"])) --> ["public"].  
visibility_ast(terminal(visibility,["private"])) -->["private"].

id(terminal(id,[ID])) --> [ID],{id(ID)}.

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
%@    true.


% ?- ast_generation("polynomial.src","polynomial_ugly_ast.txt").
%@    true.


/*

Tree Traversing

tree of structure
node(name, children_list).
terminal(name, value). for terminal nodes

basic traversal of my AST structure requires just the two predicates formated as such

traverse(terminal(_,_)).
traverse(node(_,Children)):-
    command_traverse(Children).

command_traverse([],_).
command_traverse([H|T],Path):-
    traverse(H,Path),
    command_traverse(T,Path).

all extra cases are added to traverse where it may intercept the traversal
depending on the current node it is on.
*/


scopes([program,struct,impl_def,func_def,if,while]).


%scope(id,type).


%Base cases
traverse([],_Path,_O).
traverse([[]],_Path,_O).
traverse(terminal(_Name,[_Value]),_Path,_O).
% cases where it enters a new scope
traverse(node(program,Children),Rest,O):-
    length([_Value|Rest],_L),
    command_traverse(Children,
		     [scope(global,program)|Rest],O).
traverse(node(Name,[node(func_head,[terminal(id,[Value])|Parms])|T]),Rest,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],L),
    %write([Value|Rest]),write(L),nl,
    command_traverse([node(func_head,[terminal(id,[Value])|Parms])|T],
		     [scope(Value,Name)|Rest],O).
traverse(node(func_body,[]),Path,O):-
    write(O,['Warning, function has no body: ',Path]),write(O,'\n'),
    command_traverse([],Path,O).
traverse(node(Name,[terminal(id,[Value])|T]),Rest,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],_L),
    command_traverse([terminal(id,[Value])|T],
		     [scope(Value,Name)|Rest],O).
/* Covers the case if if and while are considered their own scopes
traverse(node(Name,Children),Rest):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],L),
    command_traverse(Children,
		     [scope(Name,Name)|Rest]).
*/
% cases where it needs to add to the symbol table
traverse(node(expr,Children),Path,O):-
    assert_or_warn(expression(Path,(node(expr,Children))),O),
    command_traverse(Children,Path,O).
traverse(node(_,[terminal(id,[ID]),
		 node(func_call,[Params|_])]),Path,O):-
    count_func_call_params(Params,ParamCount),
    assert_or_warn(symbol(Path,func_call,ID,ParamCount),O).
traverse(node(_,[terminal(id,[ID]),
		 node(func_call,[Params|_])]),Path,O):-
    count_func_call_params(Params,ParamCount),
    assert_or_warn(symbol(Path,func_call,ID,ParamCount),O).
traverse(node(func_head,[terminal(id,[ID]),
			 Params,
			 terminal(type,[Returns])]),Path,O):-
    Path = [scope(_N,struct)|_T],
    assert_or_warn(symbol(Path,function,Returns,ID),O),
    param_decl(ID,Params,Path,O).
traverse(node(func_head,[terminal(id,[ID]),Params,
			 terminal(type,[Returns])]),Path,O):-
    assert_or_warn(symbol(Path,function,Returns,ID),O),
    parse_params(Params,Path,O).
traverse(node(inherits,[terminal(id,[ID])|Next]),[scope(Name,Scope)|Rest],O):-
    assert_or_warn(inherits(Name,ID),O),
    command_traverse([terminal(id,[ID])|Next],
		     [scope(Name,Scope)|Rest],O).
traverse(node(_Name,Children),[Current|Rest],O):-
    Current = scope(_S,struct),
    Children = [terminal(visibility,[Visibility]),
		node(var,[
		terminal(id,[ID]),
		terminal(type,[Type])|_])],
    assert_or_warn(symbol([Current|Rest],local,Visibility,Type,ID),O),
    command_traverse(Children,[Current|Rest],O).
traverse(node(var,Children),Path,O):-
    Children = [terminal(id,[ID]),terminal(type,[Type]),terminal(arr,[Size])|_],
    assert_or_warn(symbol(Path,local,arr,Type,ID,Size),O),
    command_traverse(Children,Path,O).
traverse(node(var,Children),Path,O):-
    Children = [terminal(id,[ID]),terminal(type,[Type])],
    assert_or_warn(symbol(Path,local,Type,ID),O),
    command_traverse(Children,Path,O).
traverse(node(_,Children),Path,O):-    
    command_traverse(Children,Path,O).
% error handling
traverse(E,_Path,O):-write(O,"error: "),write(E),nl,!.


command_traverse([],_,_O).
command_traverse([H|T],Path,O):-
    traverse(H,Path,O),
    command_traverse(T,Path,O).

assert_or_warn(ToAssert,O):-
    ToAssert,
    write(O,['Warning, Symbol already exists: ',ToAssert]),write(O,'\n'),!.
assert_or_warn(ToAssert,_O):-assertz(ToAssert),!.



% ?- assert_or_warn(symbol([scope("main",func_def),scope(global,program)],func_call,"printary",3)).
%@ [Warning: Symbol already exists in this scope
%@ ,symbol([scope("main",func_def),scope(global,program)],func_call,"printary",3)]   true.
%@    true.



parse_params(node(fparam,[terminal(id,[ID]),
			  terminal(type,[Type])]),
	     Path,O):-
    assert_or_warn(symbol(Path,param,Type,ID),O).
parse_params(node(fparam,[terminal(id,[ID]),
			  terminal(type,[Type]),
			  [terminal(arr,[])]]),
	     Path,O):-
    assert_or_warn(symbol(Path,param,arr,Type,ID),O).    
parse_params(node(fparam,
		  [terminal(id,[ID]),
		   terminal(type,[Type]),Param]),
	     Path,O):-    
    assert_or_warn(symbol(Path,param,Type,ID),O),
    parse_params(Param,Path,O).
parse_params(node(fparam,
		  [terminal(id,[ID]),
		   terminal(type,[Type]),
		   [terminal(arr,[])],Param]),
	     Path,O):-
    assert_or_warn(symbol(Path,param,arr,Type,ID),O),
    parse_params(Param,Path,O).


param_decl(FID,node(fparam,[terminal(id,[ID]),
			  terminal(type,[Type])]),
	     Path,O):-
    assert_or_warn(symbol(Path,param_decl,FID,Type,ID),O).
param_decl(FID,node(fparam,[terminal(id,[ID]),
			  terminal(type,[Type]),
			  [terminal(arr,[])]]),
	     Path,O):-
    assert_or_warn(symbol(Path,param_decl, arr,FID,Type,ID),O).    
param_decl(FID,node(fparam,
		  [terminal(id,[ID]),
		   terminal(type,[Type]),Param]),
	     Path,O):-    
    assert_or_warn(symbol(Path,param_decl,FID,Type,ID),O),
    param_decl(FID,Param,Path,O).
param_decl(FID,node(fparam,
		  [terminal(id,[ID]),
		   terminal(type,[Type]),
		   [terminal(arr,[])],Param]),
	     Path,O):-
    assert_or_warn(symbol(Path,param_decl,FID,Type,ID),O),
    param_decl(FID,Param,Path,O).



count_func_call_params([],0).
count_func_call_params(node(aparam,[_,Next]),C0):-
    C #= C0 - 1,
    count_func_call_params(Next,C).


solve_inheritance(O) :-
    findall([A,B],inherits(A,B),[X,Y]),
    inherit1(X,Y,O),
    inherit2(X,Y,O),
    inherit3(X,Y,O),!.
solve_inheritance(_O).

% inheriting of arity 4
inherit1([],[],_O).
inherit1([H1|T1],[H2|T2],O):-
    findall(symbol([scope(H1,S)|T],local,B,C),symbol([scope(H1,S)|T],local,B,C),Inherited),
    finalize_inherits(Inherited,H2,O),
    inherit1(T1,T2,O).

% inheriting of arity 5
inherit2([],[],_O).
inherit2([H1|T1],[H2|T2],O):-
    findall(symbol([scope(H1,S)|T],local,B,C,D),symbol([scope(H1,S)|T],local,B,C,D),Inherited),
    finalize_inherits(Inherited,H2,O),
    inherit2(T1,T2,O).

% case of inheriting arity 6 arrays
inherit3([],[],_O).
inherit3([H1|T1],[H2|T2],O):-
    findall(symbol([scope(H1,S)|T],local,B,C,D),symbol([scope(H1,S)|T],local,arr,B,C,D),Inherited),
    finalize_inherits(Inherited,H2,O),
    inherit3(T1,T2,O).


finalize_inherits([],_S,_O).
finalize_inherits([symbol([scope(_,S)|Rest],local,ID,Type)|T],NewScope,O):-
    assert_or_warn(symbol([scope(NewScope,S)|Rest],inherited,ID,Type),O),
    finalize_inherits(T,NewScope,O).
finalize_inherits([symbol([scope(_,S)|Rest],local,Visibility,ID,Type)|T],NewScope,O):-
    assert_or_warn(symbol([scope(NewScope,S)|Rest],inherited,Visibility,ID,Type),O),
    finalize_inherits(T,NewScope,O).

    
    
tree_tests(Filein,Fileout,SemanticErrorFile) :-
    open(Filein,read,S1,[]),
    open(Fileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    phrase(remove_comment_blocks(Lst3),Lst2),
    remove_inline(Lst3,Lst4),
    remove(Lst4,"\n",Lst),
    write(Lst), % for debugging
    phrase(prog_ast(AST),Lst),!,
    write(S2,AST),
    setup,
    cleanup,
    open(SemanticErrorFile,write,S3,[]),
    traverse(AST,_P,S3),!,
    solve_inheritance(S3),
    print_symtable("Current_sym_table.txt"),
    semantic_checking(AST,S3),
    close(S3),
    close(S1),
    close(S2).



setup :-
    asserta(symbol(a,b,c,d)),
    asserta(symbol(a,b,c,d,e)),
    asserta(expression(a,b)),
    asserta(inherits(a,b)).

cleanup:-
    retractall(inherits(_,_)),
    retractall(expression(_,_)),
    retractall(symbol(_,_,_,_,_,_)),
    retractall(symbol(_,_,_,_,_)),
    retractall(symbol(_,_,_,_)).
/*

declare the following
if in a struct
- variables with visibility
- functions
if in a func
- vars
if in an impl
- nothing declared but must match a struct

*/



% ?- tree_tests("bubblesort.src","bubblesort_ugly_tree.txt","bubblesort_semantic.txt").
%@ ["func","bubbleSort","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","{","let","n",":","integer",";","let","i",":","integer",";","let","j",":","integer",";","let","temp",":","integer",";","n","=","size",";","i","=","0",";","j","=","0",";","temp","=","0",";","while","(","i","<","n","-","1",")","{","j","=","0",";","while","(","j","<","n","-","i","-","1",")","{","if","(","arr","[","j","]",">","arr","[","j","+","1","]",")","then","{","temp","=","arr","[","j","]",";","arr","[","j","]","=","arr","[","j","+","1","]",";","arr","[","j","+","1","]","=","temp",";","}","else",";","j","=","j","+","1",";","}",";","i","=","i","+","1",";","}",";","}","func","printArray","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","{","let","n",":","integer",";","let","i",":","integer",";","n","=","size",";","i","=","0",";","while","(","i","<","n",")","{","write","(","arr","[","i","]",")",";","i","=","i","+","1",";","}",";","}","func","main","(",")","->","void","{","let","arr",":","integer","[","7","]",";","arr","[","0","]","=","64",";","arr","[","1","]","=","34",";","arr","[","2","]","=","25",";","arr","[","3","]","=","12",";","arr","[","4","]","=","22",";","arr","[","5","]","=","11",";","arr","[","6","]","=","90",";","printarray","(","arr",",","7",")",";","bubbleSort","(","arr",",","7",")",";","printarray","(","arr",",","7",")",";","}"]["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@ "arr""arr""arr"   true.




% ?- tree_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").
%@ ["struct","POLYNOMIAL","{","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","struct","LINEAR","inherits","POLYNOMIAL","{","private","let","a",":","float",";","private","let","b",":","float",";","public","func","build","(","A",":","float",",","B",":","float",")","->","LINEAR",";","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","struct","QUADRATIC","inherits","POLYNOMIAL","{","private","let","a",":","float",";","private","let","b",":","float",";","private","let","c",":","float",";","public","func","build","(","A",":","float",",","B",":","float",",","C",":","float",")","->","QUADRATIC",";","public","func","evaluate","(","x",":","float",")","->","float",";","}",";","impl","POLYNOMIAL","{","func","evaluate","(","x",":","float",")","->","float","{","return","(","0",")",";","}","}","impl","QUADRATIC","{","func","evaluate","(","x",":","float",")","->","float","{","let","result",":","float",";","result","=","a",";","result","=","result","*","x","+","b",";","result","=","result","*","x","+","c",";","return","(","result",")",";","}","func","build","(","A",":","float",",","B",":","float",",","C",":","float",")","->","QUADRATIC","{","let","new_function",":","QUADRATIC",";","new_function",".","a","=","A",";","new_function",".","b","=","B",";","new_function",".","c","=","C",";","return","(","new_function",")",";","}","}","impl","LINEAR","{","func","build","(","A",":","float",",","B",":","float",")","->","LINEAR","{","let","new_function",":","LINEAR",";","new_function",".","a","=","A",";","new_function",".","b","=","B",";","return","(","new_function",")",";","}","func","evaluate","(","x",":","float",")","->","float","{","let","result",":","float",";","result","=","0",".","0",";","result","=","a","*","x","+","b",";","return","(","result",")",";","}","}","func","main","(",")","->","void","{","let","f1",":","LINEAR",";","let","f2",":","QUADRATIC",";","let","counter",":","integer",";","f1","=","f1",".","build","(","2",",","3",".","5",")",";","f2","=","f2",".","build","(","-","2",".","0",",","1",".","0",",","0",".","0",")",";","counter","=","1",";","while","(","counter","<=","10",")","{","write","(","counter",")",";","write","(","f1",".","evaluate","(","counter",")",")",";","write","(","f2",".","evaluate","(","counter",")",")",";","}",";","}"]
%@ caught: error('$interrupt_thrown',repl)


% ?- tree_tests("polynomialsemanticerrors.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").




/*
%@ [
"func","bubbleSort","(","arr",":","integer","[","]",",","size",":","integer",")","->","void",
"{",
	"let","n",":","integer",";",
	"let","i",":","integer",";",
	"let","j",":","integer",";",
	"let","temp",":","integer",";","n","=","size",";",
	"i","=","0",";","j","=","0",";",
	"temp","=","0",";",
	"while","(","i","<","n","-","1",")",
	"{",
		"while","(","j","<","n","-","i","-","1",")",
		"{",
			"if","(","arr","[","j","]",">","arr","[","j","+","1","]",")",
			"then",
			"{",
				"temp","=","arr","[","j","]",";",
				"arr","[","j","]","=","arr","[","j","+","1","]",";",
				"arr","[","j","+","1","]","=","temp",";",
			"}","else",";",
			"j","=","j","+","1",";",
		"}",";",
		"i","=","i","+","1",";",
	"}",";",
"}",

"func","printArray","(","arr",":","integer","[","]",",","size",":","integer",")","->","void",
"{",
	"let","n",":","integer",";",
	"let","i",":","integer",";",
	"n","=","size",";","i","=","0",";",
	"while","(","i","<","n",")","{","write","(","arr","[","i","]",")",";",
	"i","=","i","+","1",";","}",";",
"}",

"func","main","(",")","->","void",
	"let","arr",":","integer","[","7","]",";",
	"arr","[","0","]","=","64",";","arr","[","1","]","=","34",";",
	"arr","[","2","]","=","25",";",
	"arr","[","3","]","=","12",";",
	"arr","[","4","]","=","22",";",
	"arr","[","5","]","=","11",";",
	"arr","[","6","]","=","90",";",
	"printarray","(","arr",",","7",")",";",
	"bubbleSort","(","arr",",","7",")",";",
	"printarray","(","arr",",","7",")",";",
"}"]
%@ caught: error('$interrupt_thrown',repl)
*/



/*
Asserted format

symbol(scope,source,type,identifier,value).

scope is a list of the path from root to it.
ex:[prog,"main","QUADRATIC"]
where main is a class, and quadratic is a struct in the class.

source is where it is from.
- local
- inherited
- parameter

make a prettyprint all asserted symbols to file function

*/

print_symtable(OutFile):-
    open(OutFile,write,O,[]),
    findall(inherits(A,B),inherits(A,B),Inherits),
    iterate_write(Inherits,O),
    findall(symbol(A1,B1,C1,D1,E1,F1),symbol(A1,B1,C1,D1,E1,F1),Syms1),
    iterate_write(Syms1,O),
    findall(symbol(A2,B2,C2,D2,E2),symbol(A2,B2,C2,D2,E2),Syms2),
    iterate_write(Syms2,O),
    findall(symbol(A3,B3,C3,D3),symbol(A3,B3,C3,D3),Syms3),
    iterate_write(Syms3,O),
    close(O).

iterate_write([],_).
iterate_write([H|T],O):-
    write(O,H),
    write(O,'\n'),
    iterate_write(T,O).




/*

Tests
*/

%identifiers_in_use(O) :-

all_function_call_usage(O):-
    findall(symbol(Scope,function,Returns,ID),
	    symbol(Scope,function,Returns,ID),
	    Fs),
    write(Fs),
    function_call_usage(Fs,O).
    

function_call_usage([],_O).
function_call_usage([symbol(Scope,function,_Returns,ID)|T],O) :-
    findall(ID,symbol(Scope,param,_,ID),Params1),
    findall(ID,symbol(Scope,param,arr,_,ID),Params2),
    length(Params1,Ps1),
    length(Params2,Ps2),
    symbol(_,func_call,ID,ParamsUsed),
    ParamSize #= Ps1 + Ps2,
    (ParamSize = ParamsUsed
    -> write(O,[Scope,ID,'Correct param count']),write(O,'\n');
     write(O,[Scope,ID,'Incorrect param count']),write(O,'\n')),
    function_call_usage(T,O).


% ?- symbol(S,function,R,I),findall(I,symbol(S,param,_,I),P).
%@    P = ["build","evaluate","build","evaluate","evaluate","evaluat ...","build ...","bui ...","e ...",...].

/*

Get a list of every variable declared for every scope

iterate the tree
- each time one is used in a statement,expr,etc as long as its not a var node, remove it from the list.
- return true if all are gone
- if they aren't all gone then write them to error

all_declared_vars_used(O):-
    findall(symbol(P2,_,T,Value),symbol(P2,_,T,Value),Syms1),
    findall(symbol(P2,_,_,T,Value),symbol(P2,_,_,T,Value),Syms2).

*/


%Base cases
traverse_check_decl([],_Path,_O).
traverse_check_decl([[]],_Path,_O).
traverse_check_decl(terminal(id,[Value]),Path,O):-
    (symbol(Path,_,_,Value);
    symbol(Path,_,_,_,Value);
    symbol(Path,_,_,_,Value,_)
    -> true
    ;(write(O,['undeclared ID',Path,Value]),write(O,'\n'))).
traverse_check_decl(terminal(_N,[_Value]),_Path,_O).
% cases where it enters a new scope
traverse_check_decl(node(program,Children),Rest,O):-
    length([_Value|Rest],_L),
    command_traverse_check_decl(Children,
		     [scope(global,program)|Rest],O).
traverse_check_decl(node(Name,[node(func_head,[terminal(id,[Value])|Parms])|T]),Rest,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],_L),
    ((symbol([terminal(id,[Value])|Parms],_,_,Value);
    symbol( [terminal(id,[Value])|Parms],_,_,_Value);
    symbol( [terminal(id,[Value])|Parms],_,_,_Value,_))
    -> true
    ;write(O,['undeclared Func',Value]),write(O,'\n')),
    command_traverse_check_decl([node(func_head,[terminal(id,[Value])|Parms])|T],
		     [scope(Value,Name)|Rest],O).
traverse_check_decl(node(Name,[terminal(id,[Value])|T]),Rest,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],_L),
    command_traverse_check_decl([terminal(id,[Value])|T],
		     [scope(Value,Name)|Rest],O).
traverse_check_decl(node(_,[terminal(id,[ID]),
		 node(func_call,[Params|_])]),Path,O):-
    count_func_call_params(Params,ParamCount),
    ((symbol(Path,function,_,ID),
      findall(Fps,symbol(Path,param,_,_),P1),
      findall(Fps,symbol(Path,param,_,_),P2),
      append(P1,P2,P3),
      length(P3,Pl),
      Pl = ParamCount)
    -> true
    ;(write(O,['undeclared Func',Path]),write(O,'\n'))).
traverse_check_decl(node(_,[terminal(id,[ID]),
		 node(func_call,[Params|_])]),Path,O):-
    count_func_call_params(Params,ParamCount),
    ((symbol(Path,function,_,ID),
      findall(Fps,symbol(Path,param,_,_),P1),
      findall(Fps,symbol(Path,param,_,_),P2),
      append(P1,P2,P3),
      length(P3,Pl),
      Pl = ParamCount)
    -> true
    ;write(O,['undeclared Func',Path]),write(O,'\n')).
traverse_check_decl(node(_,Children),Path,O):- 
    command_traverse_check_decl(Children,Path,O).
% error handling
traverse_check_decl(E,_Path,O):-write(O,"error: "),write(E),nl,!.


command_traverse_check_decl([],_,_O).
command_traverse_check_decl([H|T],Path,O):-
    traverse_check_decl(H,Path,O),
    command_traverse_check_decl(T,Path,O).
    


traverse_expr([],_P).
%traverse_expr(node(dot,_),_P).
traverse_expr(terminal(intnum,_),_P):-
    assertz(expr("integer")).
traverse_expr(terminal(floatnum,_),_P):-
    assertz(expr("float")).
traverse_expr(terminal(id,[Value]),P1):-
    symbol(P2,_,T,Value);
    symbol(P2,_,_,T,Value),
    subscope(P1,P2),
    write(Value),
    assertz(expr(T)).
traverse_expr(node(_Name,Children),P):-
    command_traverse_expr(Children,P).

command_traverse_expr([],_P).
command_traverse_expr([H|T],P):-
    traverse_expr(H,P),
    command_traverse_expr(T,P).

test_expr_type(O) :-
    findall(expression(P,E),expression(P,E),Exprs),
    traverse_all_expr(Exprs,O).

traverse_all_expr([],_O).
traverse_all_expr([expression(P,H)|T],O):-
    traverse_expr(H,P),
    findall(E,expr(E),Exp),
    all_same(Exp),
    retractall(expr(_)),
    traverse_all_expr(T,O).
traverse_all_expr([_H|T],O):-
    findall(E,expr(E),Exp),
    Exp = [],
    traverse_all_expr(T,O).
traverse_all_expr([H|T],O):-
    findall(E,expr(E),Exp),
    write(O,["Expression type mismatch:\n",H]),write(O,'\n'),
    write(O,Exp),write(O,'\n'),
    retractall(expr(_)),
    traverse_all_expr(T,O).

all_same([]).
all_same([_H]).
all_same([H,H|T]):-
    all_same([H|T]).

subscope(Lst,Lst).
subscope([_H|T],L2):-
    subscope(T,L2).

% ?- all_same([a,a,a,a,a]).
%@    true
%@ ;  false.


/*
% ?-     findall(E,expr(E),Exp).
%@    Exp = [intnum].
*/


/* 

Dot operator only on class type

*/


dot_op_check([],_O).
dot_op_check([[]],_O).
dot_op_check(terminal(_Name,_),_O).
dot_op_check(node(id_statement,
		   [terminal(id,[Name]),
		    node(dot,Children)]),O):-
    symbol(_,function,_,Name),
    command_dot_op_check(Children,O).
dot_op_check(node(id_statement,
		   [terminal(id,[Name]),
		    node(dot,Children)]),O):-
    write(O,["Warning, dot operator used on non function",Name]),
    command_dot_op_check(Children,O).
dot_op_check(node(_Name,Children),O):-
    command_dot_op_check(Children,O).


command_dot_op_check([],_P).
command_dot_op_check([H|T],P):-
    dot_op_check(H,P),
    command_dot_op_check(T,P).




% ?- subscope([scope("evaluate",func_def),scope("QUADRATIC",impl_def),scope(global,program)],[scope("QUADRATIC",impl_def),scope(global,program)]).
%@    true
%@ ;  false.



semantic_checking(Ast,O) :-
    write("Starting Checks"),nl,
%    Ast = Ast, O = O.
%    all_function_call_usage(O),
    test_expr_type(O),!,
    traverse_check_decl(Ast,_Path,O).
%    dot_op_check(Ast,O),!.



/*

Assembler

*/

% memory operations
mem_op(add,"+").
mem_op(sub,"-").
mem_op(mul,"*").
mem_op(div,"/").
mem_op(mod,"%").
mem_op(and,"&").
mem_op(or,"|").
mem_op(not,"!").
mem_op(ceq,"==").
mem_op(cne,"!=").
mem_op(clt,"<").
mem_op(cle,"<=").
mem_op(cgt,">").
mem_op(cge,">=").

% immediate operation
immediate_op(addi,"+").
immediate_op(subi,"-").
immediate_op(muli,"*").
immediate_op(divi,"/").
immediate_op(modi,"%").
immediate_op(andi,"&").
immediate_op(ori,"|").
immediate_op(noti,"!").
immediate_op(ceqi,"==").
immediate_op(cnei,"!=").
immediate_op(clti,"<").
immediate_op(clei,"<=").
immediate_op(cgti,">").
immediate_op(cgei,">=").
immediate_op(sl,">=").
immediate_op(sr,">=").


get_op(R1,_R2,Op,Out):-
    integer_(R1),
    immediate_op(Out,Op),!.
get_op(_R1,R2,Op,Out):-
    integer_(R2),
    immediate_op(Out,Op),!.
get_op(_R1,_R2,Op,Out):-
    mem_op(Out,Op),!.
    

% ?- get_op(R1,R2,"==",Op).
%@    R1 = "0", Op = ceqi.

% ?- get_op("1","3","==",Op).
%@    Op = ceqi.

% ?- get_op(1,32,"==",Op).
%@    Op = ceq.




% IO operations
instr(getc,_X).



% ?- read_term_from_chars("1",X).
%@ caught: error(syntax_error(unexpected_end_of_file),read_term_from_chars/2)
%@ false.



/*
encode(assign(Name,Expression),D,[Code|instr(store,Address)]):-
  lookup(Name,D,Address),
  encode_expression(Expression,D,Code).

encode_expression(number(C),_D,instr(loadc,C)).
encode_expression(name(X),D,instr(loadc,Address)):-
  lookup(X,D,Address).
*/


/* 

generate labels and space

*/

% instr(Label,Instr,R1,R2,Offset).

% space(Type,Bytes).


cell(none,r0,0).

space("integer",4).
space("float",8).
space("id",16).
space("void",1).
space(F,Sizes):-
    symbol([scope(F,struct)|_],_,_,_),
    findall(class_member(Context,Type),symbol([scope(F,struct)|_],Context,Type,_),All),
    get_all_sizes(All,Sizes),!.
space(F,S):-
    symbol(_,function,S0,F),
    space(S0,S).


get_all_sizes([],0).
get_all_sizes([class_member(local,Type)|T],S0):-
    space(Type,S1),
    S0 #= S1 + S,
    get_all_sizes(T,S).
get_all_sizes([class_member(inherited,Type)|T],S0):-
    space(Type,S1),
    S0 #= S1 + S,
    get_all_sizes(T,S).
get_all_sizes([_H|T],S):-
    get_all_sizes(T,S).


%reserve_space(symbol(S,function,R,N)).
reserve_space(symbol(S,Context,T,N)):-
    (Context = local;Context = inherited),
    cell(_,_Label, Loc0),
    space(T,Size),
    Loc #= Loc0 + Size,
    scope_code(S,Code),
    asserta(cell(Code,N,Loc)),!.
reserve_space(symbol(Scope,Context,arr,T,N,A)):-
    (Context = local;Context = inherited),
    A #>= 0,
    cell(_,_Label, Loc0),
    space(T,S),
    Next #= S * A,
    Loc #= Loc0 + Next,
    scope_code(Scope,Code),
    asserta(cell(Code,N,Loc)).
reserve_space(symbol(Scope,function,T,N)):-
    cell(_,_Label,Loc0),
    space(T,Size),
    Loc #= Loc0 + Size,
    scope_code(Scope,Code),
    asserta(cell(Code,N,Loc)).    



reserve_symbol(Id,Scope,Ref):-
    symbol(Scope,_,arr,T,Id,Arrsize),
    space(T,Size0),
    Size #= Size0 * Arrsize,
    scope_code(Scope,Code),
    \+ reserved(Code,Id,Size,_),
    gensym(symbol,Ref),
    assertz(reserved(Code,Id,Size,Ref)).
reserve_symbol(Id,Scope,Ref):-
    symbol(Scope,_,T,Id),
    space(T,Size),
    scope_code(Scope,Code),
    \+ reserved(Code,Id,Size,_),
    gensym(symbol,Ref),
    assertz(reserved(Code,Id,Size,Ref)).
reserve_symbol(Id,Scope,Ref):-
    symbol(Scope,_,_,T,Id),
    space(T,Size),
    scope_code(Scope,Code),
    \+ reserved(Code,Id,Size,_),
    gensym(symbol,Ref),
    assertz(reserved(Code,Id,Size,Ref)).
reserve_symbol(Id,Scope,Ref):-
    scope_code(Scope,Code),
    reserved(Code,Id,_Size,Ref).


% ?- symbol(S,_,_,I),reserve_symbol(I,S,Ref).

% ?- reserved(A,B,C,D).
%@    A = global, B = buf, C = 20, D = buf
%@ ;  A = scope15, B = "i", C = 4, D = symbol40
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb125, C = 4, D = symb125
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb124, C = 4, D = symb124
%@ ;  A = scope15, B = "j", C = 4, D = symbol41
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb127, C = 4, D = symb127
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb126, C = 4, D = symb126
%@ ;  A = scope15, B = "k", C = 4, D = symbol42
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb129, C = 4, D = symb129
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb130, C = 4, D = symb130
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb128, C = 4, D = symb128
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb132, C = 4, D = symb132
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb131, C = 4, D = symb131
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb133, C = 4, D = symb133
%@ ;  A = scope15, B = "m", C = 4, D = symbol43
%@ ;  A = scope15, B = "l", C = 4, D = symbol44
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb134, C = 4, D = symb134
%@ ;  A = scope15, B = "n", C = 4, D = symbol45
%@ ;  A = scope15, B = "o", C = 4, D = symbol46
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb136, C = 4, D = symb136
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb137, C = 4, D = symb137
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb135, C = 4, D = symb135
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb139, C = 4, D = symb139
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb140, C = 4, D = symb140
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb138, C = 4, D = symb138
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb141, C = 4, D = symb141
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb143, C = 4, D = symb143
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb142, C = 4, D = symb142
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb145, C = 4, D = symb145
%@ ;  A = [scope("main",func_def),scope(global,program)], B = symb144, C = 4, D = symb144.
%@    A = scope3, B = "main", C = 1, D = symbol10
%@ ;  A = scope3, B = "i", C = 4, D = symbol11
%@ ;  A = scope3, B = "j", C = 4, D = symbol12
%@ ;  A = scope3, B = "k", C = 4, D = symbol13
%@ ;  A = scope3, B = "l", C = 4, D = symbol14
%@ ;  A = scope3, B = "m", C = 4, D = symbol15
%@ ;  A = scope3, B = "n", C = 4, D = symbol16
%@ ;  A = scope3, B = "o", C = 4, D = symbol17.


test_reserve :-
    retractall(cell(_,_,_)),
    asserta(cell(none,r0,0)),
    findall(symbol(S1,local,T1,N1),symbol(S1,local,T1,N1),O1),
    findall(symbolS(S2,local,arr,T2,N2,Size),symbol(S2,local,arr,T2,N2,Size),O2),
    findall(symbol(S3,function,N,R),symbol(S3,function,N,R),O3),
    reserve_each(O1),
    reserve_each(O2),
    reserve_each(O3).


reserve_each([]).
reserve_each([H|T]):-
    reserve_space(H),
    reserve_each(T).


encode_scopes :-
    reset_gensym(scope),
    retractall(scope_code(_,_)),
    findall(S1,symbol(S1,_,_,_),Scopes1),
    findall(S2,symbol(S2,_,_,_,_),Scopes2),
    findall(S3,symbol(S3,_,_,_,_,_),Scopes3),
    treple(Scopes1,Scopes2,Scopes3,Scopes),
    assert_scope_codes(Scopes),!.



assert_scope_codes([]).
assert_scope_codes([H|T]):-
    \+ scope_code(H,_),
    gensym(scope,S),
    assertz(scope_code(H,S)),
    assert_scope_codes(T).
assert_scope_codes([_H|T]):-
    assert_scope_codes(T).

    
assembly_tests(Filein) :-
    split_delimiter(Filein,N,_R,'.'),
    write(N),
    append(N,"_ugly_tree.txt",ASTFileout0),
    atom_chars(ASTFileout,ASTFileout0),
    append(N,"_semantic.txt",Semanticout0),
    atom_chars(Semanticout,Semanticout0),
    append(N,"_moon.m",Moon0),
    atom_chars(Moon,Moon0),
    open(Filein,read,S1,[]),
    open(ASTFileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    fix_floats(Lst2,Lst3),
    phrase(remove_comment_blocks(Lst4),Lst3),
    remove_inline(Lst4,Lst5),
    remove(Lst5,"\n",Lst),
    %write(Lst), % for debugging
    phrase(prog_ast(AST),Lst),!,
    write(S2,AST),
    setup,
    cleanup,
    open(Semanticout,write,S3,[]),
    traverse(AST,_P,S3),!,
    solve_inheritance(S3),
    print_symtable("Current_sym_table.txt"),
    semantic_checking(AST,S3),
    encode_scopes,
    test_reserve,!,
    cleanup_2,
    open(Moon,write,S4,[]),
    traverse_assemble(AST,_P2,Instrs0,S4),!,
    %    write(Instrs),
    append(Instrs0,[instr(none,j,[stop])],Instrs),
    write_program(Instrs,S4),
    close(S4),
    close(S3),
    close(S1),
    close(S2).

cleanup_2 :-
    reset_gensym(if), 
    reset_gensym(while),
    reset_gensym(symbol),
    retractall(reserved(_,_,_,_)),
    assertz(reserved(global,buf,20,buf)).

write_program(I,S):-
    write_to_moon(I,S),!,
    findall(instr(Label,res,[Size]),
	    reserved(_Scope,_Id,Size,Label),I2),
    write_to_moon(I2,S),!.



% ?-     findall(instr(Label,res,[Size]),reserved(_Scope,_Id,Size,Label),I2).
%@    I2 = [instr(symbol9,res,[4])].


% ?- reserved(X,Y,Z,A).
%@    X = scope10, Y = "main", Z = 1, A = symbol5
%@ ;  X = scope10, Y = "i", Z = 4, A = symbol6
%@ ;  X = scope10, Y = "j", Z = 4, A = symbol7
%@ ;  ...


% ?- assembly_tests("polynomial.src").


% ?- space("QUADRATIC",S).
%@    S = 40.


% ?- encode_scopes.


% ?- reset_gensym(scope260).
%@    true.



% ?- scope_code(X,Y).


% ?- test_reserve,!.
%@    true.
%@    true.


% ?- cell(X,Y,Z).
%@    X = scope18, Y = "main", Z = 333
%@ ;  X = scope17, Y = "evaluate", Z = 332


% ?- reserve_space(symbol([scope("LINEAR",struct),scope(global,program)],local,"float","a")).


/*

node(arith_expr,[
  node(term,[
    node(factor,[
      terminal(intnum,["2"]),[],[]]),[]]),
  node(addop,[
    terminal(addop,["+"]),
    node(term,[
      node(factor,[
        terminal(intnum,["7"])]),[]])])])

instr(Label,Directive,[Paramteters])
instr(message,db,["Hello, World!",13,10,0])

instr(none,Directive,[Paramteters]).
instr(Label,Directive,[Paramteters]).
instr(Label,Directive,[Paramteters]).

r0-15 per register
each separate one is signified by the offset

-4(r0) and -8(r0) are separate

[0,4,8,12|T],

*/

% ?- scope_code(X,Code),cell(Code,Name,Offset).
%@    X = [scope("main",func_def),scope(global,program)], Code = scope80, Name = "main", Offset = 29
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope80, Name = "o", Offset = 28
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope80, Name = "n", Offset = 24
%@ ;  ...
%@    X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "main", Offset = 29
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "o", Offset = 28
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "n", Offset = 24
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "m", Offset = 20
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "l", Offset = 16
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "k", Offset = 12
%@ ;  X = [scope("main",func_def),scope(global,program)], Code = scope78, Name = "j", Offset = 8
%@ ;  ...


%Base cases
traverse_assemble([],_Path,[],_O).
traverse_assemble([[]],_Path,[],_O).
traverse_assemble(terminal(_Name,[_Value]),_Path,[],_O).
% cases where it enters a new scope
traverse_assemble(node(program,Children),Rest,I0,O):-
    cell(Code,"main",_Offset),
    Header = [instr(none,align,[]),instr(none,entry,[]),instr(none,j,[Code]),instr(stop,hlt,[])],
    append(Header,I,I0),
    length([_Value|Rest],_L),
    command_traverse_assemble(Children,
		     [scope(global,program)|Rest],I,O).
traverse_assemble(node(Name,[node(func_head,[terminal(id,[Value])|Parms])|T]),Rest,I1,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],_L),
    scope_code([scope(Value,Name)|Rest],Code),
    cell(Code,Value,_Offset),!,
    %write([scope(Value,Name)|Rest]),nl,
    %reserve_symbol(Value,[scope(Value,Name)|Rest],Ref),
    (Rest = [scope(Class,impl_def)|_R]
    ->I0 = [instr(Code,' ',['%','Function',Value,'Scope',Class])|I]
    ; (Rest = [scope(global,program)|_R]
      -> I0 = [instr(Code,' ',['%','Free Function',Value])|I]
      ; I0 = I)),
    command_traverse_assemble([node(func_head,[terminal(id,[Value])|Parms])|T],
			      [scope(Value,Name)|Rest],I,O),
    (Value = "main"
    -> I1 = I0
    ; append(I0,[instr(none,jr,[r15])],I1)).
traverse_assemble(node(Name,[terminal(id,[Value])|T]),Rest,I,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],_L),
    command_traverse_assemble([terminal(id,[Value])|T],
		     [scope(Value,Name)|Rest],I,O).
% general other cases
% expressions
traverse_assemble(node(expr,Children),P,I,_O):-
    encode_expression(node(expr,Children),I,P).
% if statement
traverse_assemble(node(if,[Bool,Then,Else]),P,I,O):-
    encode_expression(Bool,I1,P),
    traverse_assemble(Then,P,I2,O),
    traverse_assemble(Else,P,I3,O),
    gensym(if,ThenRef),
    gensym(if,ElseRef),
    gensym(if,ElseEnd),
    I1 = [instr(ignore,_,[_r1,BnzVal0])|_T],
    append_term_chars(BnzVal0,"(r0)",BnzVal),
    BoolEnd = [instr(none,lw,[r1,BnzVal]),
	       instr(none,bnz,[r1,ThenRef]),
	       instr(none,j,[ElseEnd])],
    ThenStart0 = [instr(ThenRef,'  ',[])|I2],
    append(ThenStart0,[instr(none,j,[ElseEnd])],ThenStart),
    ElseStart0 = [instr(ElseRef,'  ',[])|I3],
    append(ElseStart0,[instr(ElseEnd,'  ',[])],ElseStart),
    append(I1,BoolEnd,IBool),
    treple(IBool,ThenStart,ElseStart,I).
% while statement
traverse_assemble(node(while,[Bool,Do]),P,I,O):-
    encode_expression(Bool,I1,P),
    traverse_assemble(Do,P,I2,O),
    gensym(loop,WhileRefS),
    gensym(while,WhileRefE),
    I1 = [instr(_Con,_Co,[_r1,BnzVal0])|_T],
    append_term_chars(BnzVal0,"(r0)",BnzVal),
    BoolEnd = [instr(none,lw,[r1,BnzVal]),
	       instr(none,bnz,[r1,WhileRefE])],
    WhileEnd0 = [instr(none,j,[WhileRefS]),instr(WhileRefE,' ',[])],
    append(I2,WhileEnd0,WhileEnd),
    quadruple([instr(WhileRefS,' ',[])|I1],BoolEnd,I2,WhileEnd,I),!.
% assignment
traverse_assemble(node(id_statement,[terminal(id,[Value]),node(assign,[Expr])]),P,I,_O):-
    encode_expression(Expr,[instr(ignore,lw,[_,ValRef])|_T],P),
    reserve_symbol(Value,P,Ref0),
    append_term_chars(Ref0,"(r0)",L),
    (integer_(ValRef)
    ->(atom_chars(Vr,ValRef),
       Assignment = [instr(none,sub,[r1,r1,r1]),
		     instr(none,addi,[r1,r1,Vr]),
		     instr(none,sw,[L,r1])])
    ;(append_term_chars(ValRef,"(r0)",Vr),
    Assignment = [instr(none,lw,[r1,Vr]),instr(none,sw,[L,r1])])),
    append([instr(ignore,lw,[_,L])|_T],Assignment,I).
% writing
traverse_assemble(node(write,[Expr]),P,I,_O):-
    encode_expression(Expr,[instr(_C,lw,[_,Ref0])|_T],P),
    write_term_to_chars(Ref0,[],Ref),
    append(Ref,"(r0)",L0),
    atom_chars(L,L0),
    Writing = [instr(none,lw,[r1,L]),
	       instr(none,jl,[r15,putint]),
	       instr(none,sub,[r1,r1,r1]),
	       instr(none,addi,[r1,r1,10]),
	       instr(none,putc,[r1])],
    append([instr(_C,lw,[_,Ref])|_T],Writing,I).
% reading
traverse_assemble(node(read,[Expr]),P,I,_O):-
    encode_expression(Expr,[instr(_C,lw,[_,Ref])|_T],P),
    append_term_chars(Ref,"(r0)",L),
    Reading = [instr(non,jl,[getint,r1]),instr(none,sw,[L,r1])],
    append([instr(_C,lw,[_,Ref])|_T],Reading,I).
% general default case
traverse_assemble(node(_Name,Children),P,I,O):-
    command_traverse_assemble(Children,P,I,O).


command_traverse_assemble([],_P,[],_O).
command_traverse_assemble([H|T],P,I,O):-
    traverse_assemble(H,P,I1,O),
    command_traverse_assemble(T,P,I2,O),
    append(I1,I2,I).


% ?- reserved(A,B,C,D).
%@    A = [scope("main",func_def),scope(global,program)], B = symb478, C = 4, D = symb478
%@ ;  A = scope18, B = "x", C = 4, D = symbol82.



% base case of the expression is a id or number to just load the store for use
encode_expression([],_,_Scope).
encode_expression([[]],_,_Scope).
encode_expression(terminal(intnum,[V]),[instr(ignore,lw,[_R,V])],_Scope).
encode_expression(terminal(floatnum,[V]),[instr(ignore,lw,[_R,Ref])],Scope):-
    reserve_symbol(V,Scope,Ref).
encode_expression(terminal(id,[V]),[instr(ignore,lw,[_R,Ref])],Scope):-
    reserve_symbol(V,Scope,Ref).
% multop case, first will find a available loc to load its value into
% then do the expr
% end with saving into loaded store
encode_expression(node(term,[
			   node(factor,V1),
			   node(term,[
				    node(multop,[terminal(multop,[Op])]),
				    node(factor,V2)|_])]),
		  Expr,Scope):-
    command_encode_expression(V1,[instr(_L1,lw,[r1,Adr1])|_T1],Scope),!,
    command_encode_expression(V2,[instr(_L2,lw,[r2,Adr2])|_T2],Scope),!,
    encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
		       [instr(_L2,lw,[r2,Adr2])|_T2],
		       Op,Expr,Scope).
% addop cases
encode_expression(node(arith_expr,[
			   node(term,V1),
			   node(addop,[terminal(addop,[Op]),
				       node(term,V2)])]),
		  Expr,Scope):-
    command_encode_expression(V1,[instr(_L1,lw,[r1,Adr1])|_T1],Scope),!,    
    command_encode_expression(V2,[instr(_L2,lw,[r2,Adr2])|_T2],Scope),!,
    encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			 [instr(_L2,lw,[r2,Adr2])|_T2],
			 Op,Expr,Scope).
encode_expression(node(relational_expr,[
			   A1,
			   node(relop,[
				    terminal(relop,[Op]),
				    A2])]),
		  Expr,Scope):-
%    write(433322),write(A1),write(A2),nl,
    encode_expression(A1,[instr(_L1,lw,[r1,Adr1])|_T1],Scope),!,    
    encode_expression(A2,[instr(_L2,lw,[r2,Adr2])|_T2],Scope),!,
    encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			     [instr(_L2,lw,[r2,Adr2])|_T2],
			     Op,Expr,Scope).
encode_expression(node(_Irr,Next),I,S):-
    command_encode_expression(Next,I,S).


encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			 [instr(_L2,lw,[r2,Adr2])|_T2],
			 Op,Expr,Scope):-
    gensym(symb,Adr0),
    OpHead = [instr(ignore,lw,[_,Adr0])],
    (integer_(Adr1)
    -> (gensym(symb,Adr10),
	append_term_chars(Adr10,"(r0)",Adr100),
	atom_chars(Adr12,Adr1),
	E1 = [instr(none,sub,[r1,r1,r1]),
	      instr(none,addi,[r1,r1,Adr12]),
	      instr(none,sw,[Adr100,r1])],
	assertz(reserved(Scope,Adr10,4,Adr10)),	
	Adr11 = Adr10)
    ;(E1 = [instr(_L1,lw,[r1,Adr1])|_T1],
      Adr11 = Adr1)),
    (integer_(Adr2)
    -> (gensym(symb,Adr20),
	append_term_chars(Adr20,"(r0)",Adr200),
	atom_chars(Adr21,Adr2),
	E2 = [instr(none,sub,[r1,r1,r1]),
	      instr(none,addi,[r1,r1,Adr21]),
	      instr(none,sw,[Adr200,r1])],
	assertz(reserved(Scope,Adr20,4,Adr20)),
	Adr22 = Adr20)
    ;(E2 = [instr(_L2,lw,[r2,Adr2])|_T2],
      Adr22 = Adr2)),
    get_op(Adr11,Adr22,Op,OpInstr),
    space("integer",Size),
    assertz(reserved(Scope,Adr0,Size,Adr0)),
    append_term_chars(Adr0,"(r0)",L),
    append_term_chars(Adr11,"(r0)",L1),
    append_term_chars(Adr22,"(r0)",L2),
    OpExpr = [
	instr(none,lw,[r1,L1]),
	instr(none,lw,[r2,L2]),
	instr(none,OpInstr,[r3,r1,r2]),
	instr(none,sw,[L,r3])],
    quadruple(OpHead,E1,E2,OpExpr,Expr),!.


append_term_chars(Term,Charlist,R):-
    write_term_to_chars(Term,[],Ref),
    append(Ref,Charlist,L0),
    atom_chars(R,L0).


% ?- append_term_chars(symbol,"(r0)",O).
%@    O = 'symbol(r0)'.

get_register(R):-
    atom_chars(R,[r,R]),
    retract(register(R0)),
    register(R0).


% ?- atom_chars(X,"r1322").
%@    X = r1322.

% ?- write_term_to_chars(scope2,[],C).


% ?- reserve_symbol(Id,Scope,Ref).



% ?- reserved(A,B,C,D).


command_encode_expression([],_I,_S).
command_encode_expression([H|T],I,S):-
    encode_expression(H,I,S),
    command_encode_expression(T,I,S).


/*


node(relational_expr,[
  node(arith_expr,[
    node(term,[
      node(factor,[terminal(id,["z"]),[],[]]),[]])]),
  node(relop,[
    terminal(relop,["<="]),
    node(arith_expr,[
      node(term,[
        node(factor,[terminal(intnum,["10"])]),[]])])])])


node(relational_expr,[
  node(arith_expr,[
    node(term,[
      node(factor,[
	terminal(id,["counter"]),[],[]]),[]])]),
  node(relop,[
    terminal(relop,["<="]),
    node(arith_expr,[
      node(term,[
	node(factor,[
	  terminal(intnum,["10"])]),[]])])])])

*/




write_to_moon([],_O).
write_to_moon([instr(Label,Directive,Params)|T],O):-
    \+ Label = ignore,
    (Label = none
    -> true
    ; write(O,Label)),
    write(O,'   '),
    write(O,Directive),
    write(O,'   '),
 %   write(Params),nl,
    write_params(Params,O),!,
    write(O,'\n'),
    write_to_moon(T,O).
write_to_moon([instr(ignore,_Directive,_Params)|T],O):-
    write(O,'\n'),
    write_to_moon(T,O).


write_params([],_O).
write_params([P],O):-write(O,P).
write_params([H1,['(','r',N,')']],O):-
    write(O,H1),
    write(O,['(','r',N,')']).
write_params([H1,['(','r',N,')']|_T],O):-
    write(O,H1),
    write(O,['(','r',N,')']),
    write(O,',').
write_params([H|T],O):-
    write(O,H),
    write(O,','),
    write_params(T,O).
    

/*


% ?- assembly_tests("bubblesort.src").
%@ "bubblesort"["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@ false.



% ?- assembly_tests("polynomial.src").
%@ "polynomial"["evaluate",scope("POLYNOMIAL",impl_def),scope(global,program)]3
%@ ["evaluate",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["evaluate",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@    true.


% ?- assembly_tests("simplemain.src").


?- encode_expression(node(relational_expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),node(relop,[terminal(relop,["<="]),node(arith_expr,[node(term,[node(factor,[terminal(intnum,["10"])]),[]])])])]),S,P).
%@ 433322node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])])node(arith_expr,[node(term,[node(factor,[terminal(intnum,["10"])]),[]])])
%@    S = [instr(ignore,lw,[_A,symb353]),instr(ignore,lw,[r1,symbol28]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,'10']),instr(none,sw,['symb354(r0)',r1]),instr(none,lw,[r1,'symbol28(r0)']),instr(none,lw,[r2,'symb354(r0)']),instr(none,cle,[r3,...]),instr(none,sw,[...])], P = [scope("main",func_def),scope(global,program)].


node(relational_expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),node(relop,[terminal(relop,["<="]),node(arith_expr,[node(term,[node(factor,[terminal(intnum,["10"])]),[]])])])])


%@    true.


?- assembly_tests("hello.src","hello_ugly_tree.txt","hello_semantic.txt").


?- assembly_tests("exprs.src").
%@ "exprs"["main",scope(global,program)]2
%@ "Starting Checks"
%@ [scope("main",func_def),scope(global,program)]
%@ 433322node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]])])node(arith_expr,[node(term,[node(factor,[terminal(intnum,["3"])]),[]])])
%@ 8822882node(relational_expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["3"])]),[]])])]),node(relop,[terminal(relop,[">="]),node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])])])])
%@ 433322node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["3"])]),[]])])])node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])])
%@    true.


?- assembly_tests("simple2.src").
%@ caught: error(existence_error(procedure,assembly_tests/3),assembly_tests/3)


?- encode_expression(node(relational_expr,[node(arith_expr,[node(term,[node(factor,[


node(relational_expr,[
  node(arith_expr,[
    node(term,[node(factor,[terminal(intnum,["1"])]),[]]),
   node(addop,[terminal(addop,["+"]),
     node(term,[node(factor,[terminal(intnum,["3"])]),[]])])]),
  node(relop,[terminal(relop,[">="]),
  node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])])])])


?- scope_code(X,Y).
%@    X = [scope("main",func_def),scope(global,program)], Y = scope4.


?- command_encode_expression([node(factor,[terminal(intnum,["4"])]),[]],I,S).
%@ false.



?- cell(C,V,Offset).
%@    C = scope7, V = "main", Offset = 29
%@ ;  C = scope7, V = "o", Offset = 28
%@ ;  C = scope7, V = "n", Offset = 24
%@ ;  C = scope7, V = "m", Offset = 20
%@ ;  C = scope7, V = "l", Offset = 16
%@ ;  C = scope7, V = "k", Offset = 12
%@ ;  ...
%@    C = none, V = r0, Offset = 0.


% ?- expression(S,E).

% ?- expression(S,E),write(E),nl,encode_expression(E,I,S),write(I),nl.


*/


compile(Filein) :-
    split_delimiter(Filein,N,_R,'.'),
    % Setup the file names to output to
    append(N,"_ugly_tree.txt",ASTFileout0),
    atom_chars(ASTFileout,ASTFileout0),
    append(N,"_symbol_table.txt",Table0),
    atom_chars(Table,Table0),
    append(N,"_semantic.txt",Semanticout0),
    atom_chars(Semanticout,Semanticout0),
    append(N,"_moon.m",Moon0),
    atom_chars(Moon,Moon0),
    append(N,"_parse_errors.txt",ErrorFile0),
    atom_chars(ErrorFile,ErrorFile0),
    open(Filein,read,S1,[]),
    open(ASTFileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    fix_floats(Lst2,Lst3),
    phrase(remove_comment_blocks(Lst4),Lst3),
    remove_inline(Lst4,Lst5),
    open(ErrorFile,write,S5,[]),
    append(Lst5,['$'],Lst6),
%    table_driven_parse(Lst6,[start,'$'],0,S5),!,
    remove(Lst5,"\n",Lst),
    %write(Lst), % for debugging
    phrase(prog_ast(AST),Lst),!,
    write(S2,AST),
    setup,
    cleanup,
    open(Semanticout,write,S3,[]),
    traverse(AST,_P,S3),!,
    solve_inheritance(S3),
    print_symtable(Table),
    semantic_checking(AST,S3),
    encode_scopes,
    test_reserve,!,
    cleanup_2,
    open(Moon,write,S4,[]),
    traverse_assemble(AST,_P2,Instrs0,S4),!,
    %    write(Instrs),
    append(Instrs0,[instr(none,j,[stop])],Instrs),
    write_program(Instrs,S4),
    close(S5),
    close(S4),
    close(S3),
    close(S1),
    close(S2).


/*

?- compile("bubblesort.src").


?- compile("polynomial.src").


?- compile("exprs.src").
%@ "Starting Checks"
%@    true.



?- compile("exprs_someerrors.src").
%@ "Starting Checks"
%@    true.


?- compile("hello.src").
%@ "Starting Checks"
%@    true.


*/
