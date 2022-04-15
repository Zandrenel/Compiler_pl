/*===========================================================
Author: Alexander De Laurentiis

===========================================================*/

:-use_module(library(files)).
:-use_module(library(charsio)).
:-use_module(library(clpz)).
:-use_module(library(dcgs)).
:-use_module(library(lists)).
:-use_module(library(builtins)).
:-use_module(library(terms)).
:-use_module(library(gensym)).


:- dynamic(symbol/4).
:- dynamic(symbol/5).
:- dynamic(symbol/6).
:- dynamic(inherits/2).
:- dynamic(expression/2).
:- dynamic(expr/1).
:- dynamic(scope_code/2).
:- dynamic(cell/3).







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


% ?- analyze_file("bubblesort.src","bubblesort_semantic_analyzed.txt").


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
% ?- float_("123.01323e-833").
%@    true.
%@    true.
% ?- float_("1.01323e-833").
%@    true.
%@    true.
% ?- float_(".01323e-833").
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
%@ false.
%@ false.
%@ false.
%@    true.

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



% A sequence of characters
seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

% for current readability but likely to change
%word(X) --> X,{token(X,_)}.
word(X) --> seq(X).

% definitions of whitespace or delimiters
% Unique Case for idnests vs floats
%whitespace(F) --> F,{float_(F),!}.
whitespace([I,'.']) --> {I='0'},"0",".".
whitespace([I,'.']) --> {integer_(I)},I,".".

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
whitespace(X) -->{X=[]}, " ", whitespace(_Z).
whitespace(X) -->{X=[]}, "\t", whitespace(_Z).
whitespace(X) -->{X=[]}, "\r", whitespace(_Z).
 

% a test case
% ?- phrase(wordlist(X),"==\t+\t\n|\t(\t;\tif\n \t\t \t"),!,remove(X,[],Lst).
%@    X = [[],"==",[],[],[],"+",[],[],[],...|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.

% ?- phrase(wordlist(X),"  arr[0] = 0;arr[1] = -34.0;252.032e5; 0.932e+32;arr1.arr[1] = 34;"),!,remove(X,[],Lst),write(Lst).
%@ ["arr","[","0","]","=","0",";","arr","[","1","]","=","-","34",".","0",";","252",".","032e5",";","0.","932e","+","32",";","arr1",".","arr","[","1","]","=","34",";"]   X = [[],[],[],[],"arr","[","0","]",[],[]|...], Lst = ["arr","[","0","]","=","0",";","arr ...","[ ...",...|...]
%@ ;  false.


% ?- phrase(wordlist(X),"if 3 <> 2 then v := true ;w := false else v := true endif"),!,remove(X,[],Lst),write(Lst).
%@ ["if","3","<",">","2","then","v",":","=","true",";","w",":","=","false","else","v",":","=","true","endif"]   X = ["if",[],"3",[],[],"<",[],">",[],[]|...], Lst = ["if","3","<",">","2","then","v",":","= ...",...|...]
%@ ;  ...

% ?- float_("0.92e+32").
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
factor_ast(node(factor,[terminal(floatnum,[F])])) --> seq(F).

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



% ?- phrase(prog_ast(X),["struct","POLYNOMIAL","{","public","func","evaluate","(","x",":","float",")","->","float",";","}",";"]).
%@    X = node(program,[node(struct,[terminal(id,["POLYNOMIAL ..."]),[],node(struct_decl,[terminal(visibility,["pub ..."]),node(member,[node(func_decl,[...])])])])])
%@ ;  ...

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


%scope(id,type)

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
    write([Value|Rest]),write(L),nl,
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
traverse(node(func_head,[terminal(id,[ID]),Params,terminal(type,[Returns])]),Path,O):-
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


% ?- findall([A,B],inherits(A,B),[X,Y]).
%@ caught: error(existence_error(procedure,inherits/2),inherits/2)
%@    X = ["QUADRATIC","POLYNOMIAL"], Y = ["LINEAR","POLYNOMIAL"].
%@    C = [["QUADRATIC","POLYNOMIAL"],["LINEAR","POLYNOMIAL"]].

% ?- findall(B,inherits(A,B),C).

% ?- symbol(A,inherited,C,D,E).
%@ false.
    
    
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
%    retractall(symbol(_,_,_,_)),
%    retractall(symbol(_,_,_,_,_)),
%    retractall(symbol(_,_,_,_,_,_)),
%    retractall(expression(_,_)),
%    retractall(inherits(_,_)),
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


%@ caught: error(existence_error(procedure,dynamic/1),dynamic/1)


% ?- asserta(symbol(apple,apple,apple,apple)).


% ?- symbol(_,_,_,_).


% ?- tree_tests("bubblesort.src","bubblesort_ugly_tree.txt","bubblesort_semantic.txt").



%@ caught: error(syntax_error(incomplete_reduction),read_term/3:1)
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


% ?- tree_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").

/*
?- open("expr_test.txt",write,O),findall(expression(_,E),expression(_,E),Expr),write(O,Expr),close(O).

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
%@ ;  ...

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

% ?- tree_tests("bubblesort.src","bubblesort_ugly_tree.txt","bubblesort_semantic.txt").
%@ ["func","bubbleSort","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","{","let","n",":","integer",";","let","i",":","integer",";","let","j",":","integer",";","let","temp",":","integer",";","n","=","size",";","i","=","0",";","j","=","0",";","temp","=","0",";","while","(","i","<","n","-","1",")","{","while","(","j","<","n","-","i","-","1",")","{","if","(","arr","[","j","]",">","arr","[","j","+","1","]",")","then","{","temp","=","arr","[","j","]",";","arr","[","j","]","=","arr","[","j","+","1","]",";","arr","[","j","+","1","]","=","temp",";","}","else",";","j","=","j","+","1",";","}",";","i","=","i","+","1",";","}",";","}","func","printArray","(","arr",":","integer","[","]",",","size",":","integer",")","->","void","{","let","n",":","integer",";","let","i",":","integer",";","n","=","size",";","i","=","0",";","while","(","i","<","n",")","{","write","(","arr","[","i","]",")",";","i","=","i","+","1",";","}",";","}","func","main","(",")","->","void","{","let","arr",":","integer","[","7","]",";","arr","[","0","]","=","64",";","arr","[","1","]","=","34",";","arr","[","2","]","=","25",";","arr","[","3","]","=","12",";","arr","[","4","]","=","22",";","arr","[","5","]","=","11",";","arr","[","6","]","=","90",";","printarray","(","arr",",","7",")",";","bubbleSort","(","arr",",","7",")",";","printarray","(","arr",",","7",")",";","}"]["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@    true.



% ?- tree_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").
%@ ["evaluate",scope("POLYNOMIAL",impl_def),scope(global,program)]3
%@ ["evaluate",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["evaluate",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["main",scope(global,program)]2
%@ "Starting Checks"   true.




% ?- tree_tests("polynomialsemanticerrors.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").
%@ caught: error(existence_error(source_sink,"polynomialsemanticerrors.src"),open/4)


semantic_checking(Ast,O) :-
    write("Starting Checks"),nl,
    Ast = Ast, O = O.
%    all_function_call_usage(O),
%    test_expr_type(O),!.
%    dot_op_check(Ast,O),!.


% ?- semantic_checking("semantic_errors.txt").
%@ "Starting Checks"0_1063
%@ 0_1063
%@ 0_1063
%@    true.


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

:- dynamic(reserved/4).

reserve_symbol(Id,Scope,Ref):-
    symbol(Scope,_,arr,T,Id,Arrsize),
    space(T,Size0),
    Size #= Size * Arrsize,
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
%@    S = [scope("main",func_def),scope(global,program)], I = "main", Ref = symbol10
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "i", Ref = symbol11
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "j", Ref = symbol12
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "k", Ref = symbol13
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "l", Ref = symbol14
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "m", Ref = symbol15
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "n", Ref = symbol16
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "o", Ref = symbol17.
%@    S = [scope("main",func_def),scope(global,program)], I = "main", Ref = symbol10
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "i", Ref = symbol11
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "j", Ref = symbol12
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "k", Ref = symbol13
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "l", Ref = symbol14
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "m", Ref = symbol15
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "n", Ref = symbol16
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "o", Ref = symbol17.
%@    S = [scope("main",func_def),scope(global,program)], I = "main", Ref = symbol10
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "main", Ref = symbol10
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "i", Ref = symbol11
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "i", Ref = symbol11
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "j", Ref = symbol12
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "j", Ref = symbol12
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "k", Ref = symbol13
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "k", Ref = symbol13
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "l", Ref = symbol14
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "l", Ref = symbol14
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "m", Ref = symbol15
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "m", Ref = symbol15
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "n", Ref = symbol16
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "n", Ref = symbol16
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "o", Ref = symbol17
%@ ;  S = [scope("main",func_def),scope(global,program)], I = "o", Ref = symbol17.

% ?- reserved(A,B,C,D).
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

    
assembly_tests(Filein,ASTFileout,Semanticout) :-
    open(Filein,read,S1,[]),
    open(ASTFileout,write,S2,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst2),
    phrase(remove_comment_blocks(Lst3),Lst2),
    remove_inline(Lst3,Lst4),
    remove(Lst4,"\n",Lst),
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
    open("moontestout.m",write,S4,[]),
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
%@ caught: error(syntax_error(incomplete_reduction),read_term/3:1)
%@    X = scope10, Y = "main", Z = 1, A = symbol5
%@ ;  X = scope10, Y = "i", Z = 4, A = symbol6
%@ ;  X = scope10, Y = "j", Z = 4, A = symbol7
%@ ;  ...


% ?- assembly_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").



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
%@ ;  ...
%@    X = scope18, Y = "main", Z = 333
%@ ;  X = scope17, Y = "evaluate", Z = 332
%@ ;  ...


% ?- reserve_space(symbol([scope("LINEAR",struct),scope(global,program)],local,"float","a")).


% ?- retractall(cell(test,_)), reserve_space(symbol(S,local,T,N)).
%@ caught: error(syntax_error(incomplete_reduction),read_term/3:1)
%@ 4
%@ 4
%@    T = "integer"
%@ ;  8
%@ 8
%@ T = "float"
%@ ;  16
%@ 16
%@ T = "id".


% ?- retractall(cell(test,_)), reserve_space(symbol(S,local,arr,T,N,A)).
%@    S = [scope("main",func_def),scope(global,program)], T = "integer", clpz:(4*A#=_A), clpz:(A in 0..sup), clpz:(_A in 0..sup)
%@ ;  S = [scope("main",func_def),scope(global,program)], T = "float", clpz:(8*A#=_A), clpz:(A in 0..sup), clpz:(_A in 0..sup)
%@ ;  S = [scope("main",func_def),scope(global,program)], T = "id", clpz:(16*A#=_A), clpz:(A in 0..sup), clpz:(_A in 0..sup)
%@ ;  ...
%@ _3900   T = int, clpz:(4*A#=_A), clpz:(A in 1..sup), clpz:(_A in 4..sup)
%@ ;  _3900T = float, clpz:(8*A#=_A), clpz:(A in 1..sup), clpz:(_A in 8..sup)
%@ ;  _3900T = id, clpz:(16*A#=_A), clpz:(A in 1..sup), clpz:(_A in 16..sup).


% ?- symbol(A,B,C,D,E,F).
%@ false.


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
    cell(Code,"main",Offset),
    Header = [instr(none,align,[]),instr(none,entry,[]),instr(none,j,[Code]),instr(stop,hlt,[])],
    append(Header,I,I0),
    length([_Value|Rest],_L),
    command_traverse_assemble(Children,
		     [scope(global,program)|Rest],I,O).
traverse_assemble(node(Name,[node(func_head,[terminal(id,[Value])|Parms])|T]),Rest,I0,O):-
    scopes(Scopes),
    member(Name,Scopes),
    length([Value|Rest],L),
    write([scope(Value,Name)|Rest]),write(L),nl,
    scope_code([scope(Value,Name)|Rest],Code),
    cell(Code,Value,Offset),!,
    write("===Function"),write(Code),write(Offset),nl,
    I0 = [instr(Code,' ',[])|I],
    command_traverse_assemble([node(func_head,[terminal(id,[Value])|Parms])|T],
			      [scope(Value,Name)|Rest],I,O).
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
    I1 = [instr(ignore,_,[BnzVal])|_T],
    BoolEnd = [instr(none,bnz,[BnzVal,ElseRef])],
    ThenStart = [instr(ThenRef,add_to_next,[])|I2],
    ElseStart = [instr(ElseRef,add_to_next,[])|I3],
    append(I1,BoolEnd,IBool),
    treple(IBool,ThenStart,ElseStart,I).
% while statement
traverse_assemble(node(while,[Bool,Do]),P,I,O):-
    write(Bool),nl,
    encode_expression(Bool,I1,P),
%    write("-=-=-=-=-=--="),nl,
%    write(Do),nl,
    traverse_assemble(Do,P,I2,O),
    gensym(loop,WhileRefS),
    gensym(while,WhileRefE),
%    write("-=-=-="),nl,
%    write(Do),nl,
    I1 = [instr(_Con,_Co,[BnzVal])|_T],
    BoolEnd = [instr(none,bnz,[BnzVal,WhileRefE])],
    WhileBody = [instr(WhileRefS,add_to_next,[])|I2],
    WhileEnd0 = [instr(none,jump,[WhileRefS]),instr(WhileRefE,continue,[])],
    append(I2,WhileEnd0,WhileEnd),
    quadruple(I1,BoolEnd,WhileBody,WhileEnd,I).
% assignment
traverse_assemble(node(id_statement,[terminal(id,[Value]),node(assign,[Expr])]),P,I,O):-
    encode_expression(Expr,[instr(ignore,lw,[_,ValRef])|_T],P),
    reserve_symbol(Value,P,Ref0),
    convert_adr(Ref0,"(r0)",L),
    (integer_(ValRef)
    ->(atom_chars(Vr,ValRef),
       Assignment = [instr(none,sub,[r1,r1,r1]),
		     instr(none,addi,[r1,r1,Vr]),
		     instr(none,sw,[L,r1])])
    ;(convert_adr(ValRef,"(r0)",Vr),
    Assignment = [instr(none,lw,[r1,Vr]),instr(none,sw,[L,r1])])),
    append([instr(ignore,lw,[_,Ref])|_T],Assignment,I).
% writing
traverse_assemble(node(write,[Expr]),P,I,_O):-
    write(6767),write(node(write,[Expr])),nl,
    encode_expression(Expr,[instr(_C,lw,[_,Ref0])|_T],P),
    write(E),nl,
    write(88),write(Expr),nl,
    write(88),write([instr(_C,lw,[_,Ref0])|_T]),nl,
    write_term_to_chars(Ref0,[],Ref),
    append(Ref,"(r0)",L0),
    atom_chars(L,L0),
    Writing = [instr(none,lw,[r1,L]),instr(none,jl,[r15,putint])],
    append([instr(_C,lw,[_,Ref])|_T],Writing,I).
% reading
traverse_assemble(node(read,[Expr]),P,I,_O):-
    encode_expression(Expr,[instr(_C,lw,[_,Ref])|_T],P),
    convert_adr(Ref,"(r0)",L),
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
encode_expression(terminal(id,[V]),[instr(ignore,lw,[_R,Ref])],Scope):-
    %scope_code(Scope,C),
    %    cell(_C,V,Offset),
    reserve_symbol(V,Scope,Ref).
% multop case, first will find a available loc to load its value into
% then do the expr
% end with saving into loaded store
/*encode_expression(node(term,[
			   node(factor,V1),
			   node(term,[
				    node(multop,[terminal(multop,[Op])]),
				    node(factor,V2)])]),
		  Expr,Scope):-
    write(1),write(Op),nl,write(V1),nl,write(V2),nl,nl,
    command_encode_expression(V1,E1,Scope),!,
    command_encode_expression(V2,E2,Scope),!,
    OpHead = [instr(none,lw,[Adr])],
    E1 = [instr(_L1,lw,[Adr1])|_T1],
    E2 = [instr(_L2,lw,[Adr2])|_T2],
    OpExpr = [instr(none,Op,[Adr,Adr1,Adr2])],
    quadruple(OpHead,E1,E2,OpExpr,Expr).*/
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
		       Expr,Scope).
% addop cases
encode_expression(node(arith_expr,[
			   node(term,V1),
			   node(addop,[terminal(addop,[Op]),
				       node(term,V2)])]),
		  Expr,Scope):-
    command_encode_expression(V1,[instr(_L1,lw,[r1,Adr1])|_T1],Scope),!,    
    command_encode_expression(V2,[instr(_L2,lw,[r2,Adr2])|_T2],Scope),!,
    gensym(symb,Adr0),
    encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			 [instr(_L2,lw,[r2,Adr2])|_T2],
			 Expr,Scope).
/*encode_expression(node(expr,[node(arith_expr,[
				      node(term,V1)],
				  node(addop,[terminal(addop,[Op]),
					      node(term,V2)]))]),
		  Expr,Scope):-
    write(2),write(Op),nl,write(V1),nl,write(V2),nl,nl,
    command_encode_expression(V1,E1,Scope),!,    
    command_encode_expression(V2,E2,Scope),!,
    OpHead = [instr(none,lw,[Adr])],
    E1 = [instr(_L1,lw,[Adr1])|_T1],
    E2 = [instr(_L2,lw,[Adr2])|_T2],
    OpExpr = [instr(none,Op,[Adr,Adr1,Adr2])],
    quadruple(OpHead,E1,E2,OpExpr,Expr).*/
encode_expression(node(relational_expr,[
			   node(arith_expr,[Term]),
			   node(relop,[
				    terminal(relop,[Op]),
				    Arith_expr])]),
		  Expr,Scope):-
    encode_expression(Term,[instr(_L1,lw,[r1,Adr1])|_T1],Scope),!,    
    encode_expression(Arith_expr,[instr(_L2,lw,[r2,Adr2])|_T2],Scope),!,
    encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			     [instr(_L2,lw,[r2,Adr2])|_T2],
			     Expr,Scope).
encode_expression(node(relational_expr,[
			   A1,
			   node(relop,[
				    terminal(relop,[Op]),
				    A2])]),
		  Expr,Scope):-
    write(433322),write(A1),write(A2),nl,
    encode_expression(A1,[instr(_L1,lw,[r1,Adr1])|_T1],Scope),!,    
    encode_expression(A2,[instr(_L2,lw,[r2,Adr2])|_T2],Scope),!,
    encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			     [instr(_L2,lw,[r2,Adr2])|_T2],
			     Expr,Scope).
encode_expression(node(_Irr,Next),I,S):-
    command_encode_expression(Next,I,S).


encode_expr_instructions([instr(_L1,lw,[r1,Adr1])|_T1],
			 [instr(_L2,lw,[r2,Adr2])|_T2],
			 Expr,Scope):-
    gensym(symb,Adr0),
    OpHead = [instr(ignore,lw,[_,Adr0])],
    (integer_(Adr1)
    -> (gensym(symb,Adr10),
	convert_adr(Adr10,"(r0)",Adr100),
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
	convert_adr(Adr20,"(r0)",Adr200),
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
    convert_adr(Adr0,"(r0)",L),
    convert_adr(Adr11,"(r0)",L1),
    convert_adr(Adr22,"(r0)",L2),
    OpExpr = [
	instr(none,lw,[r1,L1]),
	instr(none,lw,[r2,L2]),
	instr(none,OpInstr,[r3,r1,r2]),
	instr(none,sw,[L,r3])],
    quadruple(OpHead,E1,E2,OpExpr,Expr),!.

convert_adr(Symbol,Register,R):-
    write_term_to_chars(Symbol,[],Ref),
    append(Ref,Register,L0),
    atom_chars(R,L0).


% ?- convert_adr(symbol,"(r0)",O).
%@    O = 'symbol(r0)'.

get_register(R):-
    atom_chars(R,[r,R]),
    retract(register(R0)),
    register(R0).

:- dynamic(register/1).

setup_registers:-
    retractall(register(_)),
    assertz(register(0)),
    assertz(register(1)),
    assertz(register(2)),
    assertz(register(3)),
    assertz(register(4)),
    assertz(register(5)),
    assertz(register(6)),
    assertz(register(7)),
    assertz(register(8)),
    assertz(register(9)),
    assertz(register(10)),
    assertz(register(11)),
    assertz(register(12)),
    assertz(register(13)),
    assertz(register(14)),
    assertz(register(15)).

% ?- atom_chars(X,"r1322").
%@    X = r1322.

% ?- write_term_to_chars(scope2,[],C).


% ?- reserve_symbol(Id,Scope,Ref).
%@ caught: error(syntax_error(incomplete_reduction),read_term/3:1)

% ?- reserved(A,B,C,D).
%@    A = scope10, B = "main", C = 1, D = symbol5
%@ ;  A = scope10, B = "i", C = 4, D = symbol6
%@ ;  A = scope10, B = "j", C = 4, D = symbol7
%@ ;  A = scope10, B = "k", C = 4, D = symbol8.


command_encode_expression([],_I,_S).
command_encode_expression([H|T],I,S):-
%    write(H),nl,nl,
    encode_expression(H,I,S),
    command_encode_expression(T,I,S).


/*

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
write_to_moon([instr(Label1,add_to_next,[]),instr(Label2,_Directive,Params)|T],O):-
    \+ Label2 = ignore,
    write(O,Label1),
%    write(Directive),
    write(O,'   '),
    write_params(Params,O),!,
    write(O,'\n'),
    write_to_moon(T,O).
write_to_moon([instr(Label1,add_to_next,[]),instr(Label2,Directive,Params)|T],O):-
    \+ Label2 = ignore,
    write(O,Label1),
    write(O,'   '),
    write(O,Directive),
    write_params(Params,O),!,
    write(O,'\n'),
    write_to_moon(T,O).
write_to_moon([instr(Label,add_to_next,[]),instr(ignore,Directive,Params)|T],O):-
    write(O,'\n'),
    write_to_moon([instr(Label,add_to_next,[])|T],O).
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
write_to_moon([instr(ignore,Directive,Params)|T],O):-
    write(O,'\n'),
    write_to_moon(T,O).


write_params([],_O).
write_params([P],O):-write(O,P).
write_params([H1,['(','r',N,')']],O):-
    write(O,H1),
    write(O,['(','r',N,')']).
write_params([H1,['(','r',N,')']|T],O):-
    write(O,H1),
    write(O,['(','r',N,')']),
    write(O,',').
write_params([H|T],O):-
    write(O,H),
    write(O,','),
    write_params(T,O).
    

/*


% ?- assembly_tests("bubblesort.src","bubblesort_ugly_tree.txt","bubblesort_semantic.txt").
%@ 


% ?- assembly_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").


% ?- assembly_tests("simplemain.src","simplemain_ugly_tree.txt","simplemain_semantic.txt").
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@ [scope("main",func_def),scope(global,program)]2
%@ "===Function"scope313
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["10"])]),[]])])]),[]])])
%@ _32643
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["10"])]),[]])])]),[]])
%@ 88[instr(ignore,lw,[_32662,symb120]),instr(ignore,lw,[r1,symbol7]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,10]),instr(none,sw,[symb121(r0),r1]),instr(none,lw,[r1,symbol7(r0)]),instr(none,lw,[r2,symb121(r0)]),instr(none,add,[r3,r1,r2]),instr(none,sw,[symb120(r0),r3])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])])
%@ _35625
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])
%@ 88[instr(ignore,lw,[_35644,symb123]),instr(ignore,lw,[r1,symbol7]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,1]),instr(none,sw,[symb124(r0),r1]),instr(none,lw,[r1,symbol7(r0)]),instr(none,lw,[r2,symb124(r0)]),instr(none,add,[r3,r1,r2]),instr(none,sw,[symb123(r0),r3])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])])
%@ _35287
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])
%@ 88[instr(ignore,lw,[_35306,symb132]),instr(ignore,lw,[r1,symbol7]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,1]),instr(none,sw,[symb133(r0),r1]),instr(none,lw,[r1,symbol7(r0)]),instr(none,lw,[r2,symb133(r0)]),instr(none,add,[r3,r1,r2]),instr(none,sw,[symb132(r0),r3])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])])
%@ _32643
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])
%@ 88[instr(ignore,lw,[_32662,symb138]),instr(ignore,lw,[r1,symbol7]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,1]),instr(none,sw,[symb139(r0),r1]),instr(none,lw,[r1,symbol7(r0)]),instr(none,lw,[r2,symb139(r0)]),instr(none,add,[r3,r1,r2]),instr(none,sw,[symb138(r0),r3])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["10"])]),[]])])]),[]])])
%@ _27681
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["10"])]),[]])])]),[]])
%@ 88[instr(ignore,lw,[_27700,symb144]),instr(ignore,lw,[r1,symbol7]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,10]),instr(none,sw,[symb145(r0),r1]),instr(none,lw,[r1,symbol7(r0)]),instr(none,lw,[r2,symb145(r0)]),instr(none,add,[r3,r1,r2]),instr(none,sw,[symb144(r0),r3])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])])
%@ _30663
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])
%@ 88[instr(ignore,lw,[_30682,symb147]),instr(ignore,lw,[r1,symbol7]),instr(none,sub,[r1,r1,r1]),instr(none,addi,[r1,r1,1]),instr(none,sw,[symb148(r0),r1]),instr(none,lw,[r1,symbol7(r0)]),instr(none,lw,[r2,symb148(r0)]),instr(none,add,[r3,r1,r2]),instr(none,sw,[symb147(r0),r3])]
%@ node(relational_expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),node(relop,[terminal(relop,["<="]),node(arith_expr,[node(term,[node(factor,[terminal(intnum,["10"])]),[]])])])])
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),[]])])
%@ _34149
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_34168,symbol9])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),[]])])
%@ _32000
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["z"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_32019,symbol9])]
%@    true.


?- split_delimiter("hello.src",H,T,'.'), append(H,"_pretty_tree.txt",A),open(A,read,S,[]),close(S).
%@ caught: error(domain_error(source_sink,"hello_pretty_tree.txt"),open/4)



?- assembly_tests("hello.src","hello_ugly_tree.txt","hello_semantic.txt").


?- assembly_tests("exprs.src","exprs_ugly_tree.txt","exprs_semantic.txt").
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@ [scope("main",func_def),scope(global,program)]2
%@ "===Function"scope629
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["j"]),[],[]]),[]])]),[]])])
%@ _21031
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["j"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_21050,symbol25])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]])])
%@ _52198
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_52217,symbol29])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]])])
%@ _49288
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_49307,symbol29])]
%@ node(relational_expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["3"])]),[]])])]),node(relop,[terminal(relop,[">="]),node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])])])])
%@ 433322node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["3"])]),[]])])])node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])])
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]])])
%@ _54512
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_54531,symbol24])]
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]])])
%@ _49672
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_49691,symbol24])]
%@    true.


?- assembly_tests("simple2.src","simple2_ugly_tree.txt","simple2_semantic.txt").
%@ ["main",scope(global,program)]2
%@ "Starting Checks"
%@ [scope("main",func_def),scope(global,program)]2
%@ "===Function"scope1613
%@ 6767node(write,[node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]])]),[]])])
%@ _12489
%@ 88node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["x"]),[],[]]),[]])]),[]])
%@ 88[instr(ignore,lw,[_12508,symbol68])]
%@    true.

?- encode_expression(node(relational_expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["3"])]),[]])])]),node(relop,[terminal(relop,[">="]),node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])])])]),I,S).
%@ false.
%@ false.
%@ false.

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
%@    S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["21"])]),[]])]),[]])
%@ ;  S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,...)]),[]])])]),[]])
%@ ;  S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["j"]),[],[]]),[]])]),[]])
%@ ;  S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(floatnum,[["\"W ..."]])]),[]])]),[]])
%@ ;  S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[node(arith_expr,...)]),[]])])]),[]])
%@ ;  S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["l"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(id,...),...]),[]])])]),[]])
%@ ;  S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[node(arith_expr,[node(term,[node(factor,[...]),[]]),node(addop,[terminal(...)|...])])]),node(term,[node(multop,[terminal(multop,[...])]),node(factor,[terminal(floatnum,...)]),[]])]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[terminal(floatnum,...)]),node(term,[...])])])]),[]]).

% ?- expression(S,E),write(E),nl,encode_expression(E,I,S),write(I),nl.
%@ node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["21"])]),[]])]),[]])
%@ [instr(none,lw,["21"])]
%@    S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["21"])]),[]])]),[]]), I = [instr(none,lw,["21"])]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,["4"])]),[]])])]),[]])
%@ [instr(ignore,lw,[_1006]),instr(none,lw,[4]),instr(none,lw,["4"]),instr(none,addi,[_1035,4,"4"]),instr(_1006,res,[4]),instr(none,sw,[_1006,"(r0)",_1035])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(intnum,...)]),[]])])]),[]]), I = [instr(ignore,lw,[_A]),instr(none,lw,[4]),instr(none,lw,["4"]),instr(none,addi,[_B,4,"4"]),instr(_A,res,[4]),instr(none,sw,[_A,"(r ...",_B])]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["j"]),[],[]]),[]])]),[]])
%@ [instr(none,lw,[8])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["j"]),[],[]]),[]])]),[]]), I = [instr(none,lw,[8])]
%@ ;  [instr(none,lw,[8])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["j"]),[],[]]),[]])]),[]]), I = [instr(none,lw,[8])]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(floatnum,[[""World!""]])]),[]])]),[]])
%@ node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["3"])]),[]]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["2"])]),[]]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])])]),[]])])])]),[]])])]),[]])
%@ [instr(ignore,lw,[_1362]),instr(none,lw,[4]),instr(ignore,lw,["0"]),instr(none,lw,["3"]),instr(ignore,lw,[_1168]),instr(none,lw,["2"]),instr(none,lw,["1"]),instr(none,subi,[_1197,"2","1"]),instr(_1168,res,[4]),instr(none,sw,[_1168,"(r0)",_1197]),instr(none,subi,[_1279,"3",_1168]),instr("0",res,[4]),instr(none,sw,["0","(r0)",_1279]),instr(none,addi,[_1394,4,"0"]),instr(_1362,res,[4]),instr(none,sw,[_1362,"(r0)",_1394])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[node(arith_expr,...)]),[]])])]),[]]), I = [instr(ignore,lw,[_A]),instr(none,lw,[4]),instr(ignore,lw,["0"]),instr(none,lw,["3"]),instr(ignore,lw,[_B]),instr(none,lw,["2"]),instr(none,lw,["1"]),instr(none,subi,[_C,...]),instr(_B,res,[...]),instr(...)|...]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["l"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(id,["l"]),[],[]]),[]])])]),[]])
%@ [instr(ignore,lw,[_1014]),instr(none,lw,[16]),instr(none,lw,[16]),instr(none,add,[_1043,16,16]),instr(_1014,res,[4]),instr(none,sw,[_1014,"(r0)",_1043])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["l"]),[],[]]),[]]),node(addop,[terminal(addop,["+"]),node(term,[node(factor,[terminal(id,...),...]),[]])])]),[]]), I = [instr(ignore,lw,[_A]),instr(none,lw,[16]),instr(none,lw,[16]),instr(none,add,[_B,16,16]),instr(_A,res,[4]),instr(none,sw,[_A,"(r ...",_B])]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["7"])]),[]]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[terminal(intnum,["2"])]),[]])])])]),node(term,[node(multop,[terminal(multop,["*"])]),node(factor,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["1"])]),[]]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["2"])]),node(term,[node(multop,[terminal(multop,["*"])]),node(factor,[terminal(intnum,["9"])]),[]])])])]),[]])])])]),[]])])]),[]])
%@ [instr(ignore,lw,[_1511]),instr(ignore,lw,["0"]),instr(none,lw,["7"]),instr(none,lw,["2"]),instr(none,subi,[_1231,"7","2"]),instr("0",res,[4]),instr(none,sw,["0","(r0)",_1231]),instr(ignore,lw,[_1399]),instr(none,lw,["1"]),instr(ignore,lw,[_1317]),instr(none,lw,["2"]),instr(none,lw,["9"]),instr(none,muli,[_1346,"2","9"]),instr(_1317,res,[4]),instr(none,sw,[_1317,"(r0)",_1346]),instr(none,subi,[_1428,"1",_1317]),instr(_1399,res,[4]),instr(none,sw,[_1399,"(r0)",_1428]),instr(none,muli,[_1543,"0",_1399]),instr(_1511,res,[4]),instr(none,sw,[_1511,"(r0)",_1543])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[node(arith_expr,[node(term,[node(factor,[...]),[]]),node(addop,[terminal(...)|...])])]),node(term,[node(multop,[terminal(multop,[...])]),node(factor,[node(arith_expr,...)]),[]])])]),[]]), I = [instr(ignore,lw,[_A]),instr(ignore,lw,["0"]),instr(none,lw,["7"]),instr(none,lw,["2"]),instr(none,subi,[_B,"7","2"]),instr("0",res,[4]),instr(none,sw,["0","(r0)",...]),instr(ignore,lw,[_C]),instr(none,lw,[...]),instr(...)|...]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]])
%@ [instr(none,lw,[24])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]]), I = [instr(none,lw,[24])]
%@ ;  [instr(none,lw,[24])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["n"]),[],[]]),[]])]),[]]), I = [instr(none,lw,[24])]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]])
%@ [instr(none,lw,[4])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]]), I = [instr(none,lw,[4])]
%@ ;  [instr(none,lw,[4])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]])]),[]]), I = [instr(none,lw,[4])]
%@ ;  node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[terminal(intnum,["1"])]),[]])])]),[]])
%@ [instr(ignore,lw,[_1006]),instr(none,lw,[4]),instr(none,lw,["1"]),instr(none,subi,[_1035,4,"1"]),instr(_1006,res,[4]),instr(none,sw,[_1006,"(r0)",_1035])]
%@ S = [scope("main",func_def),scope(global,program)], E = node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,["i"]),[],[]]),[]]),node(addop,[terminal(addop,["-"]),node(term,[node(factor,[terminal(intnum,...)]),[]])])]),[]]), I = [instr(ignore,lw,[_A]),instr(none,lw,[4]),instr(none,lw,["1"]),instr(none,subi,[_B,4,"1"]),instr(_A,res,[4]),instr(none,sw,[_A,"(r ...",_B])].




% ?- E = node(term,[
           node(factor,[terminal(intnum,["2"])]),
           node(term,[
             node(multop,[terminal(multop,["*"])]),
             node(factor,[terminal(intnum,["2"])]),[]])]),
	     encode_expression(E,I,S),!.
%@    E = node(term,[node(factor,[terminal(intnum,["2"])]),node(term,[node(multop,[terminal(multop,["*"])]),node(factor,[terminal(intnum,["2"])]),[]])]), I = [instr(ignore,lw,[_A]),instr(none,lw,["2"]),instr(none,lw,["2"]),instr(none,muli,[_B,"2","2"]),instr(_A,res,[4]),instr(none,sw,[_A,"(r ...",_B])].




to find addresses do so by 
 ?- scope_code(S,C),cell(C,Name,Offset).
 %@    S = [scope("POLYNOMIAL",struct),scope(global,program)], C = scope244, Name = "evaluate", Offset = 180
 %@ ;  S = [scope("LINEAR",struct),scope(global,program)], C = scope245, Name = "evaluate", Offset = 204
 %@ ;  S = [scope("LINEAR",struct),scope(global,program)], C = scope245, Name = "build", Offset = 196
 %@ ;  S = [scope("LINEAR",struct),scope(global,program)], C = scope245, Name = "b", Offset = 16
 %@ ;  S = [scope("LINEAR",struct),scope(global,program)], C = scope245, Name = "a", Offset = 8
 %@ ;  S = [scope("QUADRATIC",struct),scope(global,program)], C = scope246, Name = "evaluate", Offset = 252
 %@ ;  S = [scope("QUADRATIC",struct),scope(global,program)], C = scope246, Name = "build", Offset = 244
 %@ ;  ...


*/


