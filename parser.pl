:- module(parser, [parse/2]).

parse(String, Ast) :- tokenize(String, Tokens), parse(Tokens, [end_of_file], Ast).

op_prec(or, 0).

op_prec(and, 1).

op_prec(equal, 2).
op_prec(different, 2).

op_prec(less, 3).
op_prec(less_equal, 3).
op_prec(greater, 3).
op_prec(greater_equal, 3).

op_prec(plus, 4).
op_prec(minus, 4).

op_prec(multiply, 5).
op_prec(divide, 5).
op_prec(modulus, 5).

op_prec(negation, 6).

parse(Tokens, Rest, Ast) :- parse_level(0, Tokens, Rest, Ast).

parse_level(Level, Tokens, Rest, Ast) :-
    Level < 7,
    !,
    NextLevel is Level + 1,
    parse_level(NextLevel, Tokens, [Op | Tokens0], Left),
    (op_prec(Op, Level) ->
         parse_level(Level, Tokens0, Rest, Right),
         Ast = apply(Op, Left, Right) ;
     Rest = [Op | Tokens0], Ast = Left).

parse_level(_, [lparen | Tokens], Rest, Ast) :-
    parse_level(0, Tokens, [rparen | Rest], Ast).

parse_level(_, [minus | Tokens], Rest, apply(minus, Ast)) :-
    parse_level(0, Tokens, Rest, Ast).

parse_level(_, [integer(Int) | Rest], Rest, Int).

%% Should parse function call here.

parse_level(_, [identifier(Id) | Rest], Rest, Id).
