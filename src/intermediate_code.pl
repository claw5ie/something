:- module(intermediate_code, [emit_im_code/1]).

emit_im_code(Ast) :-
    list_to_assoc([], Enviroment),
    emit_body(
        flags(0, 0, -1), Enviroment, Ast
    ).

emit_im_code(Flags, Enviroment, expr(Expr), Dest) :-
    emit_im_code(Flags, Enviroment, Expr, Dest).

emit_im_code(
    flags(Temp, Label, Labels),
    Enviroment,
    inline_if(Cond, IfTrue, IfFalse),
    Dest
) :-
    IfFalseLabel is Label + 1,
    EndLabel is Label + 2,
    CondTemp is Temp + 1,
    emit_im_code(
        flags(CondTemp, EndLabel, Labels),
        Enviroment,
        Cond,
        CondTemp
    ),
    format("    bz       $t~w, l~w\n", [CondTemp, IfFalseLabel]),
    emit_im_code(
        flags(CondTemp, EndLabel, Labels),
        Enviroment,
        IfTrue,
        Dest
    ),
    format("    j        l~w\nl~w:\n", [EndLabel, IfFalseLabel]),
    emit_im_code(
        flags(CondTemp, IfFalseLabel, Labels),
        Enviroment,
        IfFalse,
        Dest
    ),
    format("l~w:\n", [EndLabel]).

emit_im_code(
    Flags,
    Enviroment,
    apply(Op, Expr),
    Dest
) :-
    emit_im_code(Flags, Enviroment, Expr, Dest),
    format("    ~|~w~t~8+ ~w t~w, t~w\n", [Op, Dest, Dest]).

emit_im_code(
    flags(Temp, Label, Labels),
    Enviroment,
    apply(Op, Left, Right),
    Dest
) :-
    LeftTemp is Temp + 1,
    emit_im_code(
        flags(LeftTemp, Label, Labels),
        Enviroment,
        Left,
        LeftTemp
    ),
    emit_im_code(
        flags(LeftTemp, Label, Labels),
        Enviroment,
        Right,
        Dest
    ),
    format("    ~|~w~t~8+ $t~w, $t~w, $t~w\n", [Op, Dest, LeftTemp, Dest]).

emit_im_code(_, _, integer(Value), Dest) :-
    format("    move     $t~w, ~w\n", [Dest, Value]).

emit_im_code(Flags, Enviroment, fun_call(Id, ExprList), Dest) :-
    get_assoc(Id, Enviroment, label(Label)),
    emit_expr_list(Flags, Enviroment, ExprList, Args),
    format("    call     $t~w, ~w, l~w\n", [Dest, Args, Label]).

emit_im_code(_, Enviroment, identifier(Id), Dest) :-
    get_assoc(Id, Enviroment, temp(Temp)),
    format("    move     $t~w, $t~w\n", [Dest, Temp]).

emit_im_code(_, _, return(), _) :-
    format("    ret\n").

emit_im_code(flags(Temp, Label, Labels), Enviroment, return(Expr), _) :-
    ReturnTemp is Temp + 1,
    emit_im_code(flags(ReturnTemp, Label, Labels), Enviroment, Expr, ReturnTemp),
    format("    ret      $t~w\n", [ReturnTemp]).

emit_im_code(Flags, Enviroment, assign(Id, Expr), _) :-
    get_assoc(Id, Enviroment, temp(Dest)),
    emit_im_code(Flags, Enviroment, Expr, Dest).

emit_im_code(
    flags(Temp, Label, Labels),
    Enviroment,
    if(Cond, IfTrue, IfFalse),
    _
) :-
    IfFalseLabel is Label + 1,
    EndLabel is Label + 2,
    CondTemp is Temp + 1,
    emit_im_code(
        flags(CondTemp, EndLabel, Labels),
        Enviroment,
        Cond,
        CondTemp
    ),
    format("    bz       $t~w, l~w\n", [CondTemp, IfFalseLabel]),
    emit_body(
        flags(CondTemp, EndLabel, Labels),
        Enviroment,
        IfTrue
    ),
    format("    j        l~w\nl~w:\n", [EndLabel, IfFalseLabel]),
    emit_body(
        flags(CondTemp, IfFalseLabel, Labels),
        Enviroment,
        IfFalse
    ),
    format("l~w:\n", [EndLabel]).

emit_im_code(flags(Temp, Label, _), Enviroment, while(Cond, Body), _) :-
    StartLabel is Label + 1,
    CondLabel is Label + 2,
    BreakLabel is Label + 3,
    format("    j        l~w\nl~w:\n", [CondLabel, StartLabel]),
    emit_body(flags(Temp, BreakLabel, BreakLabel), Enviroment, Body),
    format("l~w:\n", [CondLabel]),
    CondTemp is Temp + 1,
    emit_im_code(flags(CondTemp, BreakLabel, -1), Enviroment, Cond, CondTemp),
    format("    bnz      $t~w, l~w\nl~w:\n", [CondTemp, StartLabel, BreakLabel]).

emit_im_code(flags(_, _, BreakLabel), _, break, _) :-
    format("    j        l~w\n", [BreakLabel]).

emit_body(Flags, Enviroment, [Statement | Body]) :-
    emit_and_add_statement_to_enviroment(
        Flags, Enviroment, Statement, result(NewFlags, NewEnviroment)
    ),
    emit_body(NewFlags, NewEnviroment, Body).

emit_body(_, _, []).

emit_and_add_statement_to_enviroment(
    flags(Temp, Label, Labels),
    Enviroment,
    vardef(_, Id),
    result(flags(VarTemp, Label, Labels), NewEnviroment)
) :-
    VarTemp is Temp + 1,
    put_assoc(Id, Enviroment, temp(VarTemp), NewEnviroment).

emit_and_add_statement_to_enviroment(
    flags(Temp, Label, Labels),
    Enviroment,
    vardef(_, Id, Expr),
    result(Flags, NewEnviroment)
) :-
    VarTemp is Temp + 1,
    Flags = flags(VarTemp, Label, Labels),
    emit_im_code(Flags, Enviroment, Expr, VarTemp),
    put_assoc(Id, Enviroment, temp(VarTemp), NewEnviroment).

emit_and_add_statement_to_enviroment(
    flags(Temp, Label, Labels),
    Enviroment,
    defun(_, Id, Params, Body),
    result(Flags, NewEnviroment)
) :-
    FunLabel is Label + 1,
    put_assoc(Id, Enviroment, label(FunLabel), Enviroment0),
    collect_params(Temp, Enviroment0, Params, result(NewTemp, NewEnviroment)),
    Flags = flags(NewTemp, FunLabel, Labels),
    format("l~w:\n", FunLabel),
    emit_body(Flags, Enviroment, Body).

emit_and_add_statement_to_enviroment(
    Flags, Enviroment, Statement, result(Flags, Enviroment)
) :-
    emit_im_code(Flags, Enviroment, Statement, -1).

collect_params(
    Temp, Enviroment, [param(_, Id) | Params], Result
) :-
    NewTemp is Temp + 1,
    put_assoc(Id, Enviroment, temp(NewTemp), NewEnviroment),
    collect_params(NewTemp, NewEnviroment, Params, Result).

collect_params(Temp, Enviroment, [], result(Temp, Enviroment)).

emit_expr_list(
    flags(Temp, Label, Labels),
    Enviroment,
    [Expr | ExprList],
    [Arg | Temps]
) :-
    ArgTemp is Temp + 1,
    emit_im_code(
        flags(ArgTemp, Label, Labels),
        Enviroment,
        Expr,
        ArgTemp
    ),
    emit_expr_list(
        flags(ArgTemp, Label, Labels),
        Enviroment,
        ExprList,
        Temps
    ),
    atom_concat('$t', ArgTemp, Arg).

emit_expr_list(_, _, [], []).
