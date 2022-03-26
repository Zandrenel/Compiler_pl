/*===========================================================
Author: Alexander De Laurentiis

===========================================================*/

:-use_module(library(files)).
:-use_module(library(charsio)).
:-use_module(library(clpz)).
:-use_module(library(dcgs)).
:-use_module(library(lists)).
%:-use_module(library(builtins)).

:-use_module(library(error)).
%:- dynamic(symbol/4).





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
whitespace(X) -->{X=[]}, " ", whitespace(Z).
whitespace(X) -->{X=[]}, "\t", whitespace(Z).
whitespace(X) -->{X=[]}, "\r", whitespace(Z).
 

% a test case
% ?- phrase(wordlist(X),"==\t+\t\n|\t(\t;\tif\n \t\t \t"),!,remove(X,[],Lst).
%@    X = [[],"==",[],[],[],"+",[],[],[],...|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.

% ?- phrase(wordlist(X),"  arr[0] = 0;arr[1] = -34.0;252.032e5; 0.932e+32;arr1.arr[1] = 34;"),!,remove(X,[],Lst),write(Lst).
%@ ["arr","[","0","]","=","0",";","arr","[","1","]","=","-","34",".","0",";","252",".","032e5",";","0.","932e","+","32",";","arr1",".","arr","[","1","]","=","34",";"]   X = [[],[],[],[],"arr","[","0","]",[],[]|...], Lst = ["arr","[","0","]","=","0",";","arr ...","[ ...",...|...]
%@ ;  false.

% ?- float_("0.92e+32").
%@    true.



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


relop_ast(terminal(relop,["="])) --> ["="].
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
traverse(node(_,Children):-
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
traverse([[]],Path,_O).
traverse(terminal(Name,[Value]),_Path,_O).
% cases where it enters a new scope
traverse(node(program,Children),Rest,O):-
    length([Value|Rest],L),
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
    length([Value|Rest],L),
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
    Path = [scope(N,struct)|T],
    assert_or_warn(symbol(Path,function,Returns,ID),O),
    param_decl(ID,Params,Path,O).
traverse(node(func_head,[terminal(id,[ID]),Params,
			 terminal(type,[Returns])]),Path,O):-
    assert_or_warn(symbol(Path,function,Returns,ID),O),
    parse_params(Params,Path,O).
traverse(node(inherits,[terminal(id,[ID])|Next]),[scope(Name,Scope)|Rest],O):-
    assert_or_warn(inherits(Name,ID),O),
    command_traverse([terminal(id,[Value])|T],
		     [scope(Name,Scope)|Rest],O).
traverse(node(Name,Children),[Current|Rest],O):-
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
    phrase(prog_ast(AST),Lst),!,
    write(S2,AST),
    %setup,
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
/*

setup :-
    asserta(symbol(a,b,c,d)),
    asserta(symbol(a,b,c,d,e)),
    asserta(expression(a,b)),
    asserta(inherits(a,b)).
*/
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
%@ ["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@ caught: error(existence_error(procedure,inherits/2),inherits/2)


%@ ["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@    true.
%@ ["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@ caught: error(existence_error(procedure,inherits/2),inherits/2)



% ?- tree_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").
%@ ["evaluate",scope("POLYNOMIAL",impl_def),scope(global,program)]3
%@ ["evaluate",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["evaluate",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["main",scope(global,program)]2
%@ "Starting Checks"0_97871
%@ 0_97871
%@ 0_97871
%@    true.



/*
?- open("expr_test.txt",write,O),findall(expression(_,E),expression(_,E),Expr),write(O,Expr),close(O).
%@    O = '$stream'(0x55bce9f28e50), Expr = [expression(_A,node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(intnum,["0 ..."])]),[]])]),[]])),expression(_B,node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,[...]),[],...]),[]])]),[]])),expression(_C,node(expr,[node(arith_expr,[node(term,[node(factor,[terminal(id,...),...]),node(term,[...])]),node(addop,[terminal(addop,[...]),node(...)])]),[]])),expression(_D,node(expr,[node(arith_expr,[node(term,[node(factor,[...]),node(...)]),node(addop,[terminal(...)|...])]),[]])),expression(_E,node(expr,[node(arith_expr,[node(term,[node(...)|...])]),[]])),expression(_F,node(expr,[node(arith_expr,[node(term,...)]),[]])),expression(_G,node(expr,[node(arith_expr,[...]),[]])),expression(_H,node(expr,[node(...)|...])),expression(_I,node(expr,...)),expression(...)|...].

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
    

function_call_usage([],O).
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
traverse_expr(terminal(Name,[Value]),P1):-
    Name = id,
    symbol(P2,_,T,Value);
    symbol(P2,_,_,T,Value),
    subscope(P1,P2),
    write(Value),
    assertz(expr(T)).
traverse_expr(node(Name,Children),P):-
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
subscope([H|T],L2):-
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
dot_op_check(terminal(Name,_),_O).
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
%@ ["bubbleSort",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["printArray",scope(global,program)]2
%@ ["main",scope(global,program)]2
%@ ["failure",scope(global,program)]2
%@ ["failure2",scope(global,program)]2
%@ "Starting Checks""arr""arr""arr"   true.


% ?- tree_tests("polynomial.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").
%@ ["evaluate",scope("POLYNOMIAL",impl_def),scope(global,program)]3
%@ ["evaluate",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["evaluate",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["main",scope(global,program)]2
%@ "Starting Checks"   true.


% ?- tree_tests("polynomialsemanticerrors.src","polynomial_ugly_tree.txt","polynomial_semantic.txt").
%@ ["evaluate",scope("POLYNOMIAL",impl_def),scope(global,program)]3
%@ ["build",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["evaluate",scope("LINEAR",impl_def),scope(global,program)]3
%@ ["evaluate",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["build2",scope("QUADRATIC",impl_def),scope(global,program)]3
%@ ["f",scope(global,program)]2
%@ ["f",scope(global,program)]2
%@ ["f",scope(global,program)]2
%@ ["f3",scope(global,program)]2
%@ [terminal(arr,["2"]),terminal(arr,["3"])]
%@ ["main",scope(global,program)]2
%@ [terminal(arr,["3"]),[]]
%@ [terminal(arr,["2"]),[terminal(arr,["3"]),[]]]
%@ "Starting Checks"   true.

semantic_checking(Ast,O) :-
    write("Starting Checks"),
    all_function_call_usage(O),
    test_expr_type(O),!,
    dot_op_check(Ast,O),!.


% ?- semantic_checking("semantic_errors.txt").
%@ "Starting Checks"0_1063
%@ 0_1063
%@ 0_1063
%@    true.
