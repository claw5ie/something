:- module(typechecker, [typecheck/1]).

typecheck(Ast) :-
    list_to_assoc([], Enviroment),
    typecheck_statements(
        flags(not_in_fun, not_in_loop, void_type), Enviroment, Ast
    ).

typecheck(
    flags(IsInFun, IsInLoop, ReturnType),
    Enviroment,
    while(Cond, Body),
    void_type
) :-
    typecheck(flags(IsInFun, IsInLoop, ReturnType), Enviroment, Cond, bool_type),
    typecheck_statements(flags(IsInFun, in_loop, ReturnType), Enviroment, Body).

typecheck(Flags, Enviroment, if(Cond, IfTrue, IfFalse), void_type) :-
    typecheck(Flags, Enviroment, Cond, bool_type),
    typecheck_statements(Flags, Enviroment, IfTrue),
    typecheck_statements(Flags, Enviroment, IfFalse).

typecheck(Flags, Enviroment, expr(Expr), Type) :-
    typecheck(Flags, Enviroment, Expr, Type),
    is_non_void(Type).

typecheck(
    flags(in_fun, _, void_type),
    _,
    return(),
    void_type
).

typecheck(
    flags(in_fun, IsInLoop, ReturnType),
    Enviroment,
    return(Expr),
    void_type
) :-
    typecheck(flags(in_fun, IsInLoop, ReturnType), Enviroment, Expr, ReturnType).

typecheck(flags(_, in_loop, _), _, break, void_type).

typecheck(_, Enviroment, id(Id), Type) :-
    get_assoc(Id, Enviroment, var_type(Type)).

typecheck(Flags, Enviroment, apply(Op, Left, Right), ExprType) :-
    typecheck(Flags, Enviroment, Left, OperandType),
    typecheck(Flags, Enviroment, Right, OperandType),
    infere_expr_type(Op, OperandType, ExprType).

typecheck(Flags, Enviroment, funcall(Id, Args), ReturnType) :-
    get_assoc(Id, Enviroment, fun_type(ReturnType, ParamTypes)),
    check_arg_types(pair(Flags, Enviroment), Args, ParamTypes).

typecheck(_, _, int(_), int_type).

typecheck(_, _, bool(_), bool_type).

typecheck(_, _, apply(_, Expr), int_type) :-
    typecheck(nothing, Expr, int_type).

typecheck(Flags, Enviroment, inline_if(Cond, IfTrue, IfFalse), Type) :-
    typecheck(Flags, Enviroment, Cond, bool_type),
    typecheck(Flags, Enviroment, IfTrue, Type),
    typecheck(Flags, Enviroment, IfFalse, Type).

typecheck(Flags, Enviroment, assign(Id, Expr), void_type) :-
    get_assoc(Id, Enviroment, var_type(Type)),
    typecheck(Flags, Enviroment, Expr, Type).

typecheck_statements(Flags, Enviroment, [Statement | Body]) :-
    add_statement_to_enviroment(Flags, Enviroment, Statement, NewEnviroment),
    typecheck_statements(Flags, NewEnviroment, Body).

typecheck_statements(_, _, []).

add_statement_to_enviroment(_, Enviroment, vardef(Type, Id), NewEnviroment) :-
    put_assoc(Id, Enviroment, var_type(Type), NewEnviroment).

add_statement_to_enviroment(
    Flags,
    Enviroment,
    vardef(Type, Id, Expr),
    NewEnviroment
) :-
    typecheck(Flags, Enviroment, Expr, Type),
    put_assoc(Id, Enviroment, var_type(Type), NewEnviroment).

add_statement_to_enviroment(
    _,
    Enviroment,
    defun(Type, Id, Params, Body),
    EnvFunType
) :-
    put_defun_in_map(
        defun(Type, Id, Params), Enviroment, pair(EnvFunType, EnvParamTypes)
    ),
    typecheck_statements(flags(in_fun, not_in_loop, Type), EnvParamTypes, Body).

add_statement_to_enviroment(Flags, Enviroment, Statement, Enviroment) :-
    typecheck(Flags, Enviroment, Statement, _).

put_defun_in_map(
    defun(ReturnType, Id, Params),
    Enviroment,
    pair(EnvFunType, EnvParamTypes)
) :-
    extract_types_of_params(Params, ParamTypes),
    put_assoc(Id, Enviroment, fun_type(ReturnType, ParamTypes), EnvFunType),
    put_params_in_map(Params, EnvFunType, EnvParamTypes).

put_params_in_map([param(Type, Id) | Params], Enviroment, NewEnviroment) :-
    put_assoc(Id, Enviroment, var_type(Type), Enviroment0),
    put_params_in_map(Params, Enviroment0, NewEnviroment).

put_params_in_map([], Enviroment, Enviroment).

extract_types_of_params([param(Type, _) | Params], [Type | Types]) :-
    extract_types_of_params(Params, Types).

extract_types_of_params([], []).

check_arg_types(
    pair(Flags, Enviroment),
    [Expr | Args],
    [ExpectedType | ParamTypes]
) :-
    typecheck(Flags, Enviroment, Expr, ExpectedType),
    check_arg_types(pair(Flags, Enviroment), Args, ParamTypes).

check_arg_types(_, [], []).

is_non_void(bool_type).
is_non_void(int_type).

infere_expr_type(less_equal, int_type, bool_type).
infere_expr_type(greater_equal, int_type, bool_type).
infere_expr_type(different, Type, bool_type) :- is_non_void(Type).
infere_expr_type(or, bool_type, bool_type).
infere_expr_type(and, bool_type, bool_type).
infere_expr_type(equal, Type, bool_type) :- is_non_void(Type).
infere_expr_type(less, int_type, bool_type).
infere_expr_type(greater, int_type, bool_type).
infere_expr_type(plus, int_type, int_type).
infere_expr_type(minus, int_type, int_type).
infere_expr_type(multiply, int_type, int_type).
infere_expr_type(divide, int_type, int_type).
infere_expr_type(modulus, int_type, int_type).
