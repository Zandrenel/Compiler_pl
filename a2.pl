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
%@    true
%@ ;  false.

% ?- test_positive.
%@    true
%@ ;  false.


% ?- analyze_file("studenttestcases.src","studenttestcasesout.txt").
%@    true
%@ ;  false.
%@    true
%@ ;  false.

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
%@    X = ["==",[],"+",[],[],"\n","|",[],"( ...",[]|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.
%@    X = ["==",[],"+",[],[],"\n","|",[],"( ...",[]|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.
%@    X = ["==",[],"+",[],[],"\n","|",[],"( ...",[]|...], Lst = ["==","+","\n","|","(",";","if","\n"]
%@ ;  false.



/* Original Grammar

<START>              ::= <prog>
<prog>               ::= {{<structOrImplOrfunc>}} 
<structOrImplOrFunc> ::= <structDecl> | <implDef> | <funcDef>   
<structDecl>         ::= 'struct' 'id' [['inherits' 'id' {{',' 'id'}}]] '{' {{<visibility> <memberDecl>}} '}' ';'
<implDef>            ::= 'impl' 'id' '{' {{<funcDef>}} '}'
<funcDef>            ::= <funcHead> <funcBody> 
<visibility>         ::= 'public' | 'private' 
<memberDecl>         ::= <funcDecl> | <varDecl>  
<funcDecl>           ::= <funcHead> ';' 
<funcHead>           ::= 'func' 'id' '(' <fParams> ')' '->' <returnType> 
<funcBody>           ::= '{' {{<varDeclOrStat>}} '}'
<varDeclOrStat>      ::= <varDecl> | <statement>
<varDecl>            ::= 'let' 'id' ':' <type> {{<arraySize>}} ';'
<statement>          ::= <assignStat> ';'
                      |  'if'     '(' <relExpr> ')' 'then' <statBlock> 'else' <statBlock> ';'
                      |  'while'  '(' <relExpr> ')' <statBlock> ';'
                      |  'read'   '(' <variable> ')' ';'
                      |  'write'  '(' <expr> ')' ';'
                      |  'return' '(' <expr> ')' ';'
                      |  <functionCall> ';'
<assignStat>         ::= <variable> <assignOp> <expr>
<statBlock>          ::= '{' {{<statement>}} '}' | <statement> | EPSILON  
<expr>               ::= <arithExpr> | <relExpr>
<relExpr>            ::= <arithExpr> <relOp> <arithExpr>
<arithExpr>          ::= <arithExpr> <addOp> <term> | <term> 
<sign>               ::= '+' | '-'
<term>               ::= <term> <multOp> <factor> | <factor>
<factor>             ::= <variable>
                      |  <functionCall>
                      |  'intLit' | 'floatLit'
                      |  '(' <arithExpr> ')'
                      |  'not' <factor>
                      |  <sign> <factor>
<variable>           ::= {{<idnest>}} 'id' {{<indice>}}
<functionCall>       ::= {{<idnest>}} 'id' '(' <aParams> ')'
<idnest>             ::= 'id' {{<indice>}} '.'
                      |  'id' '(' <aParams> ')' '.'
<indice>             ::= '[' <arithExpr> ']'
<arraySize>          ::= '[' 'intNum' ']' | '[' ']'
<type>               ::= 'integer' | 'float' | 'id'
<returnType>         ::= <type> |  'void'
<fParams>            ::= 'id' ':' <type> {{<arraySize>}} {{<fParamsTail>}} | EPSILON  
<aParams>            ::= <expr> {{<aParamsTail>}} | EPSILON 
<fParamsTail>        ::= ',' 'id' ':' <type> {{<arraySize>}}
<aParamsTail>        ::= ',' <expr>
<assignOp>           ::= '='
<relOp>              ::= 'eq' | 'neq' | 'lt' | 'gt' | 'leq' | 'geq' 
<addOp>              ::= '+' | '-' | 'or' 
<multOp>             ::= '*' | '/' | 'and'

"(id-id)+(id/id)"
*/

% ?- phrase(exp,["(","id",")"]).
%@    true
%@ ;  false.


exp --> ["("], exp,[")"].
exp --> ["id"].
%exp --> exp, "+", exp.
%exp --> exp, "-", exp.
%exp --> exp, "*", exp.
%exp --> exp, "/", exp.



% ?- phrase(e0,"(id)").
%@    true
%@ ;  false.


% ?- phrase(e0,"(id+id)").
%@    true
%@ ;  false.


% ?- phrase(e0,"(id+id)+id+(id/id)").
%@ t0 e2
%@ t0 e2
%@ t0 e2
%@    true
%@ ;  false.

% ?- phrase(e0,"(id)").
%@ t0 e2
%@ t0 e2
%@    true
%@ ;  false.


e0 --> t0, e2.
t0 --> f0, t2.
f0 --> "(", e0, ")".
f0 --> "id".
e1 --> "+",t0.
e1 --> "-",t0.
t1 --> "*", f0.
t1 --> "/", f0.
e2 --> e1, e2.
e2 --> [].
t2 --> t1, t2.
t2 --> [].


% ?- phrase(e0(X),"(id+id)+id+(id/id)").
%@ 
%@ caught: error('$interrupt_thrown',repl)

% ?- phrase(e0(X),"(id)").



e0(X) --> {append(H,T,X)},t0(H), e2(T).
t0([H|T]) --> f0(H), t2(T).
f0([H|T]) --> {append(M,")",T)}, "(", e0(M), ")".
f0(X) --> "id".
e1(X) --> "+",t0(X).
e1(X) --> "-",t0(X).
t1(X) --> "*", f0(X).
t1(X) --> "/", f0(X).
e2([H|T]) --> e1(H), e2(T).
e2(X) --> [].
t2([H|T]) --> t1(H), t2(T).
t2(X) --> [].


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
letter("a") --> "a".
letter("b") --> "b".
letter("c") --> "c".
letter("d") --> "d".
letter("e") --> "e".
letter("f") --> "f".
letter("g") --> "g".
letter("h") --> "h".
letter("i") --> "i".
letter("j") --> "j".
letter("k") --> "k".
letter("l") --> "l".
letter("m") --> "m".
letter("n") --> "n".
letter("o") --> "o".
letter("p") --> "p".
letter("q") --> "q".
letter("r") --> "r".
letter("s") --> "s".
letter("t") --> "t".
letter("u") --> "u".
letter("v") --> "v".
letter("w") --> "w".
letter("x") --> "x".
letter("y") --> "y".
letter("z") --> "z".
% Uppercase
letter("A") --> "A".
letter("B") --> "B".
letter("C") --> "C".
letter("D") --> "D".
letter("E") --> "E".
letter("F") --> "F".
letter("G") --> "G".
letter("H") --> "H".
letter("I") --> "I".
letter("J") --> "J".
letter("K") --> "K".
letter("L") --> "L".
letter("M") --> "M".
letter("N") --> "N".
letter("O") --> "O".
letter("P") --> "P".
letter("Q") --> "Q".
letter("R") --> "R".
letter("S") --> "S".
letter("T") --> "T".
letter("U") --> "U".
letter("V") --> "V".
letter("W") --> "W".
letter("X") --> "X".
letter("Y") --> "Y".
letter("Z") --> "Z".

alphanum(C) --> digit(C).
alphanum(C) --> letter(C).
alphanum(C) --> "_".


character(C) --> alphanum(C).
character(" ") --> " ".


% ?- phrase(alphanum(C),"1").
%@    C = "1"
%@ ;  false.


seqAplphanum([]) --> [].
seqAplphanum([E|Es]) --> alphanum(E), seqAplphanum(Es).


seqDigit([])     --> [].
seqDigit([E|Es]) --> digit(E), seqDigit(Es).


integer([H]) --> nonzero(H).
integer([H|T]) --> nonzero(H), seqDigit(T).

% ?- phrase(integer(C),"2909").
%@    C = "2909"
%@ ;  false.
%@ false.
%@    C = "2".

fraction([H,T,Ts]) --> integer(H),".",seqDigit(T),nonzero(Ts).


% ?- phrase(fraction(C),"323322.0232232").
%@    C = ["323322","023223",'2']
%@ ;  false.

float([Frac,Int]) --> fraction(Frac), "e", sign, integer(Int).

sign --> "+".
sign --> "-".
sign --> "".


% ?- phrase(float(C),"323322.0232232e2777").
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

id([I,D]) --> letter(I), seqAplphanum(D).
id --> letter(I), seqAplphanum(D).


% ?- phrase(id(C),"Diet_struct").
%@    C = ["D",["i","e","t",_A,"s","t","r ...",...|...]]
%@ ;  false.



/*

Idea for error correction, have a symbol for every symbol that will be proposed missing and print an error if missing when parsing.

*/

start --> structOrImplOrFunc.

structOrImplOrFunc --> structDecl.
structOrImplOrFunc --> implDef.
structOrImplOrFunc --> funcDef.

structDecl --> ["struct"],["id"],["inherits"],["id"],extraId,
	       ["{"],
	       visibilityMemberDef,
	       ["}"],[";"].

structDecl --> ["struct"],["id"],
	       ["{"],
	       visibilityMemberDef,
	       ["}"],[";"].

implDef --> ["impl"],["id"],
	       ["{"],
	       funcDefPlus,
	       ["}"],[";"].


funcDefPlus --> funcDef,funcDefPlus.
funcDefPlus --> [].

funcDef --> funcHead, funcBody.

extraId --> extraId,[","],id.
extraId --> [].

visibilityMemberDecl -->  visibilityMemberDecl,visibility, memberDecl.
visibilityMemberDecl --> [].

visibility --> ["public"].
visibility --> ["private"].

memberDecl --> funcDecl.
memberDecl --> varDecl.

funcDecl --> funcHead, [";"].

funcHead --> ["func"], ["id"],["("],fParams,[")"],["->"],returnType, ["\n"].

funcBody --> ["{"], varDeclOrStatPlus , ["}"].

varDeclOrStatPlus --> varDeclOrStatPlus, varDeclOrStat.
varDeclOrStatPlus --> varDeclOrStat.

varDeclOrStat --> varDecl.
varDeclOrStat --> statement.

varDecl --> ["let"], ["id"],[":"],type,arraySizePlus,[";"].

arraySizePlus --> arraySizePlus, arraySize.
arraySizePlus --> arraySize.

statement --> assignStat, [";"].
statement --> ["if"], ["("], relExpr, [")"],
	      ["then"], statBlock,
	      ["else"], statBlock, [";"].
statement --> ["while"], ["("],relExpr, [")"],
	      statBlock,[";"].
statement --> ["read"], ["("], variable, [")"], [";"].
statement --> ["write"], ["("], expr, [")"], [";"].
statement --> ["return"], ["("], expr, [")"], [";"].
statement --> functionCall, [";"].

assignStat --> variable, assignOp, expr.

statBlock --> ["{"], statementPlus, ["}"].
statBlock --> statement.
statBlock --> [].

statementPlus --> statementPlus, statement.
statementPlus --> statement.

expr --> arithExpr.
expr --> relExpr.

relExpr --> arithExpr, relOp, arithExpr.

arithExpr --> arithExpr, addOp, term.
arithExpr --> term.

sign --> ["+"].
sign --> ["-"].

term --> term, multOp, factor.
term --> factor.

factor --> variable.
factor --> functionCall.
factor --> ["intLit"].
factor --> ["floatLit"].
factor --> ["("], arithExpr, [")"].
factor --> ["not"], factor.
factor --> sign, factor.

variable --> idnestPlus, ["id"], indicePlus.
functionCall --> idnestPlus, ["id"], ["("], aParams, [")"].

idnestPlus --> idnestPlus, idnest.
idnestPlus --> idnest.

indicePlus --> indicePlus, indice.
indicePlus --> indice.

indice --> ["["], arithExpr, ["]"].

arraySize --> ["["], ["intNum"], ["]"].
arraySize --> ["["], ["]"].

type --> ["integer"].
type --> ["float"].
type --> id.

returnType --> type.
returnType --> ["void"],{write("void")}.

fParams --> {write("fParams")}, id, [":"], type, arraySizePlus, fParamsTailPlus.
fParams --> [].


arraySizePlus --> arraySize,arraySizePlus.
arraySizePlus --> [].

fParamsTailPlus -->  {write("fParamsTailPlus")},fParamsTail, fParamsTailPlus1.
fParamsTailPlus1 --> fParamsTail.
fParamsTailPlus1 --> [].

aParams --> expr, aParamsTailPlus.
aParams --> [].

aParamsTailPlus --> aParamsTail, aParamsTailPlus.
aParamsTailPlus --> [].

fParamsTail -->{write("fParamsTail")}, [","], id, [":"], type, arraySizePlus.
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


/*

test case

*/


test_file(Filein) :-
    open(Filein,read,S1,[]),
    stream_to_charlist(S1,Lst0),
    phrase(wordlist(Lst1),Lst0),!,
    remove(Lst1,[],Lst),
    write(Lst),nl,
    remove(Lst,"\n",In),
    write(In),
    phrase(start,In),
    close(S1).



% make sure to skip newline and special characters
% clean the input of those and comments first

% ?- test_file("test.src").
%@ ["func","id","(","id",":","integer",")","->","void","
%@ ","{","
%@ ","let","id",":","integer",";","
%@ ","}",";","
%@ "]
%@ ["func","id","(","id",":","integer",")","->","void","{","let","id",":","integer",";","}",";"]"fParams"false.



% ?- phrase(start,X).
%@ 
%@ caught: error('$interrupt_thrown',repl)
