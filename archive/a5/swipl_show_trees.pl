
node(expr,[node(arith_expr,[
                    node(term,[
			     node(factor,[
				      terminal(id,["result"]),[],[]]),
			     node(term,[
				      node(multop,[
					       terminal(multop,["*"])]),
				      node(factor,[
					       terminal(id,["x"]),[],[]]),[]])]),
                    node(addop,[
			     terminal(addop,["+"]),
			     node(term,[
				      node(factor,[
					       terminal(id,["b"]),[],[]]),[]])])]),[]]).



traverse(_,[]).
traverse(Value,terminal(_Name,Value)):-write(Value),nl.
traverse(Name,node(Name,Children)):-
    write(Name),nl,
    command_traverse(Children).

command_traverse([]).
command_traverse([H|T]):-
    traverse(_N,H),
    command_traverse(T).

/*

?- traverse(_,node(expr,[node(arith_expr,[
                    node(term,[
			     node(factor,[
				      terminal(id,["result"]),[],[]]),
			     node(term,[
				      node(multop,[
					       terminal(multop,["*"])]),
				      node(factor,[
					       terminal(id,["x"]),[],[]]),[]])]),
                    node(addop,[
			     terminal(addop,["+"]),
			     node(term,[
				      node(factor,[
					       terminal(id,["b"]),[],[]]),[]])])]),[]])).
%@ expr
%@ arith_expr
%@ term
%@ factor
%@ ["result"]
%@ term
%@ multop
%@ ["*"]
%@ factor
%@ ["x"]
%@ addop
%@ ["+"]
%@ term
%@ factor
%@ ["b"]
%@    true.

*/
