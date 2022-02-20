:-use_module(library(dcgs)).






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

% ?- phrase(id(C),"Diet_struct").
%@    C = ["D",["i","e","t",_A,"s","t","r ...",...|...]]
%@ ;  false.
