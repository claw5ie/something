:- module(parser, [parse/2]).

parse(String, Ast) :-
    tokenize(String, Tokens),
    parse_top_level(Tokens, [], Ast).

parse_top_level([defun | Tokens], Rest, [Defun | Ast]) :-
    parse_defun(Tokens, Tokens0, Defun),
    parse_top_level(Tokens0, Rest, Ast).

parse_top_level([identifier(Id) | Tokens], Rest, [VarDef | Ast]) :-
    parse_vardef([identifier(Id) | Tokens], Tokens1, VarDef),
    parse_top_level(Tokens1, Rest, Ast).

parse_top_level([end_of_file], [], []).

parse_expr(Tokens, Rest, Expr) :-
    parse_level(0, Tokens, Tokens0, PossibleCond),
    parse_inline_if(Tokens0, Rest, PossibleCond, Expr).

parse_inline_if(
    [if, colon | Tokens], Rest, Cond, inline_if(Cond, IfTrue, IfFalse)
) :-
    parse_expr(Tokens, [else | Tokens0], IfTrue),
    parse_expr(Tokens0, Rest, IfFalse).

parse_inline_if(Rest, Rest, Cond, Cond).

parse_level(Level, Tokens, Rest, Ast) :-
    Level < 6,
    NextLevel is Level + 1,
    parse_level(NextLevel, Tokens, Tokens0, Left),
    fold_exprs(Level, Tokens0, Left, Rest, Ast).

parse_level(6, [lparen | Tokens], Rest, Ast) :-
    parse_expr(Tokens, [rparen | Rest], Ast).

parse_level(6, [minus | Tokens], Rest, apply(minus, Ast)) :-
    parse_expr(Tokens, Rest, Ast).

parse_level(6, [integer(Int) | Rest], Rest, integer(Int)).

parse_level(
    6, [identifier(Id), lparen | Tokens], Rest, fun_call(Id, Args)
) :-
    parse_expr_list(Tokens, [rparen | Rest], Args).

parse_level(6, [identifier(Id) | Rest], Rest, identifier(Id)).

fold_exprs(Level, [Op | Tokens], Left, Rest, Result) :-
    op_prec(Op, Level),
    !,
    NextLevel is Level + 1,
    parse_level(NextLevel, Tokens, Tokens0, Right),
    fold_exprs(Level, Tokens0, apply(Op, Left, Right), Rest, Result).

fold_exprs(_, Rest, Left, Rest, Left).

parse_expr_list(Tokens, Rest, [Expr | ExprList]) :-
    parse_expr(Tokens, Tokens0, Expr),
    consume_or_ignore(comma, Tokens0, Tokens1),
    parse_expr_list(Tokens1, Rest, ExprList).

parse_expr_list(Rest, Rest, []).

parse_body([defun | Tokens], Rest, [FunDef | Statements]) :-
    parse_defun(Tokens, Tokens0, FunDef),
    parse_body(Tokens0, Rest, Statements).

parse_body([return | Tokens], Rest, [Expr | Body]) :-
    parse_expr(Tokens, [semicolon | Tokens0], Expr),
    parse_body(Tokens0, Rest, Body).

parse_body([if | Tokens], Rest, [If | Body]) :-
    parse_if(Tokens, Tokens0, If),
    parse_body(Tokens0, Rest, Body).

parse_body([while | Tokens], Rest, [While | Body]) :-
    parse_while(Tokens, Tokens0, While),
    parse_body(Tokens0, Rest, Body).

parse_body([else | Rest], [else | Rest], []).

parse_body([semicolon | Rest], Rest, []).

parse_body(Tokens, Rest, [VarDef | Body]) :-
    parse_vardef(Tokens, Tokens1, VarDef),
    parse_body(Tokens1, Rest, Body).

parse_body(Tokens, Rest, [expr(Expr) | Body]) :-
    parse_expr(Tokens, [semicolon | Tokens1], Expr),
    parse_body(Tokens1, Rest, Body).

parse_vardef(
    [identifier(Id), colon, Type, semicolon | Rest],
    Rest,
    vardef(Type, Id)
) :-
    type_is_non_void(Type).

parse_vardef(
    [identifier(Id), colon, Type, equal | Tokens], Rest, vardef(Type, Id, Expr)
) :-
    type_is_non_void(Type),
    parse_expr(Tokens, [semicolon | Rest], Expr).

parse_defun(
    [identifier(Id), lparen | Tokens], Rest, defun(Type, Id, Params, Body)
) :-
    parse_param_list(Tokens, [rparen, colon, Type | Tokens0], Params),
    is_valid_type(Type),
    parse_body(Tokens0, Rest, Body).

parse_param_list(
    [identifier(Id), colon, Type | Tokens], Rest, [param(Type, Id) | Params]
) :-
    type_is_non_void(Type),
    consume_or_ignore(comma, Tokens, Tokens0),
    parse_param_list(Tokens0, Rest, Params).

parse_param_list(Rest, Rest, []).

parse_if(Tokens, Rest, if(Cond, IfTrue, IfFalse)) :-
    parse_expr(Tokens, [colon | Tokens1], Cond),
    parse_body(Tokens1, Tokens2, IfTrue),
    parse_else(Tokens2, Rest, IfFalse).

parse_else([else | Tokens], Rest, Else) :-
    parse_body(Tokens, Rest, Else).

parse_else(Rest, Rest, []).

parse_while(Tokens, Rest, while(Cond, Body)) :-
    parse_expr(Tokens, [colon | Tokens1], Cond),
    parse_body(Tokens1, Rest, Body).

consume_or_ignore(Token, [Token | Rest], Rest).
consume_or_ignore(_, Rest, Rest).

type_is_non_void(Type) :- Type \= void_type, is_valid_type(Type).

is_valid_type(int_type).
is_valid_type(string_type).
is_valid_type(void_type).

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
