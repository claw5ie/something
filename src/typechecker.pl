:- module(typechecker, [typecheck/1]).

typecheck(Ast) :-
    list_to_assoc([], Env),
    typecheck_statements(
        Ast, info(Env, flags(not_in_fun, not_in_loop, void_type))
    ).

typecheck(while(Cond, Body), Info, void_type) :-
    typecheck(Cond, Info, bool_type),
    Info = info(Env, flags(IsInFun, _, ReturnType)),
    typecheck_statements(
        Body, info(Env, flags(IsInFun, in_loop, ReturnType))
    ).

typecheck(if(Cond, IfTrue, IfFalse), Info, void_type) :-
    typecheck(Cond, Info, bool_type),
    typecheck_statements(IfTrue, Info),
    typecheck_statements(IfFalse, Info).

typecheck(expr(Expr), Info, Type) :-
    typecheck(Expr, Info, Type).

typecheck(return(), info(_, flags(in_fun, _, void_type)), void_type).

typecheck(return(Expr), Info, void_type) :-
    Info = info(_, flags(in_fun, _, ReturnType)),
    typecheck(Expr, Info, ReturnType).

typecheck(break, info(_, flags(_, in_loop, _)), void_type).

typecheck(id(Id), info(Env, _), Type) :-
    get_assoc(Id, Env, var_type(Type)).

typecheck(apply(Op, Left, Right), Info, ExprType) :-
    typecheck(Left, Info, OperandType),
    typecheck(Right, Info, OperandType),
    infere_expr_type(Op, OperandType, ExprType).

typecheck(funcall(Id, Args), Info, ReturnType) :-
    Info = info(Env, _),
    get_assoc(Id, Env, fun_type(ReturnType, ParamTypes)),
    check_arg_types(Info, Args, ParamTypes).

typecheck(int(_), _, int_type).

typecheck(bool(_), _, bool_type).

typecheck(apply(_, Expr), Info, int_type) :-
    typecheck(Expr, Info, int_type).

typecheck(inline_if(Cond, IfTrue, IfFalse), Info, Type) :-
    typecheck(Cond, Info, bool_type),
    typecheck(IfTrue, Info, Type),
    typecheck(IfFalse, Info, Type).

typecheck(assign(Id, Expr), Info, void_type) :-
    Info = info(Env, _),
    get_assoc(Id, Env, var_type(Type)),
    typecheck(Expr, Info, Type).

typecheck_statements([Statement | Body], Info) :-
    add_statement_to_enviroment(Statement, Info, NewInfo),
    typecheck_statements(Body, NewInfo).

typecheck_statements([], _).

add_statement_to_enviroment(
    vardef(Type, Id),
    info(Env, Flags),
    info(NewEnv, Flags)
) :-
    put_assoc(Id, Env, var_type(Type), NewEnv).

add_statement_to_enviroment(
    vardef(Type, Id, Expr),
    info(Env, Flags),
    info(NewEnv, Flags)
) :-
    typecheck(Expr, info(Env, Flags), Type),
    put_assoc(Id, Env, var_type(Type), NewEnv).

add_statement_to_enviroment(
    defun(Type, Id, Params, Body),
    info(Env, Flags),
    info(EnvFunType, Flags)
) :-
    put_defun_in_map(
        defun(Type, Id, Params), Env, pair(EnvFunType, EnvParamTypes)
    ),
    typecheck_statements(
        Body, info(EnvParamTypes, flags(in_fun, not_in_loop, Type))
    ).

add_statement_to_enviroment(Statement, Info, Info) :-
    typecheck(Statement, Info, _).

put_defun_in_map(
    defun(ReturnType, Id, Params),
    Env,
    pair(EnvFunType, EnvParamTypes)
) :-
    extract_types_of_params(Params, ParamTypes),
    put_assoc(Id, Env, fun_type(ReturnType, ParamTypes), EnvFunType),
    put_params_in_map(Params, EnvFunType, EnvParamTypes).

put_params_in_map([param(Type, Id) | Params], Env, NewEnv) :-
    put_assoc(Id, Env, var_type(Type), Env0),
    put_params_in_map(Params, Env0, NewEnv).

put_params_in_map([], Env, Env).

extract_types_of_params([param(Type, _) | Params], [Type | Types]) :-
    extract_types_of_params(Params, Types).

extract_types_of_params([], []).

check_arg_types(
    Info,
    [Expr | Args],
    [ExpectedType | ParamTypes]
) :-
    typecheck(Expr, Info, ExpectedType),
    check_arg_types(Info, Args, ParamTypes).

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
