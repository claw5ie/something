:- module(intermediate_code, [emit_im_code/1]).

%% Variables with prefix "T" are temporaries, while variables with
%% prefix "L" are labels. I don't like abbreviations, but long lines
%% get annoying after awhile.

%% This predicate takes abstract syntax tree (AST), some info,
%% destination temporary and returns last unused label.
%% Info contains the enviroment for temporaries/labels for
%% variables/functions, first unused temporary, first unused label
%% and break label (for "break" instruction). In short:
%% emit_im_code(Expr, info(Env, FirstT, FirstL, BreakL), DestT, LastL).
%% Temporary 0 is used as garbage temporary (when we want to discard
%% the result), while -1 is used as invalid label.
emit_im_code(Ast) :-
    list_to_assoc([], Env),
    emit_stmts(Ast, info(Env, 1, 0, -1), _).

emit_im_code(expr(Expr), Info, DestT, LastL) :-
    emit_im_code(Expr, Info, DestT, LastL).

emit_im_code(
    inline_if(Cond, IfTrue, IfFalse),
    info(Env, CondT, IfFalseL, _),
    DestT,
    LastL
) :-
    EndL is IfFalseL + 1,
    LastL0 is IfFalseL + 2,
    LastT is CondT + 1,

    emit_im_code(
        Cond,
        info(Env, LastT, LastL0, -1),
        CondT,
        LastL1
    ),
    format("    bz       $t~w, l~w\n", [CondT, IfFalseL]),
    emit_im_code(
        IfTrue,
        info(Env, LastT, LastL1, -1),
        DestT,
        LastL2
    ),
    format("    j        l~w\nl~w:\n", [EndL, IfFalseL]),
    emit_im_code(
        IfFalse,
        info(Env, LastT, LastL2, -1),
        DestT,
        LastL
    ),
    format("l~w:\n", [EndL]).

emit_im_code(apply(Op, Expr), Info, DestT, LastL) :-
    emit_im_code(Expr, Info, DestT, LastL),
    format("    ~|~w~t~8+ ~w t~w, t~w\n", [Op, DestT, DestT]).

emit_im_code(
    apply(Op, Left, Right),
    info(Env, LeftT, FirstL, _),
    DestT,
    LastL
) :-
    LastT is LeftT + 1,
    emit_im_code(Left, info(Env, LastT, FirstL, -1), LeftT, LastL0),
    emit_im_code(Right, info(Env, LastT, LastL0, -1), DestT, LastL),
    format(
        "    ~|~w~t~8+ $t~w, $t~w, $t~w\n",
        [Op, DestT, LeftT, DestT]
    ).

emit_im_code(int(Value), info(_, _, LastL, _), DestT, LastL) :-
    format("    move     $t~w, ~w\n", [DestT, Value]).

emit_im_code(funcall(Id, ExprList), Info, DestT, LastL) :-
    Info = info(Env, _, _, _),
    get_assoc(Id, Env, label(VarL)),
    emit_exprlist(Info, LastL, ExprList, ArgsList),
    format("    call     $t~w, ~w, l~w\n", [DestT, ArgsList, VarL]).

emit_im_code(id(Id), info(Env, _, LastL, _), DestT, LastL) :-
    get_assoc(Id, Env, temp(VarT)),
    format("    move     $t~w, $t~w\n", [DestT, VarT]).

emit_im_code(return(), _, info(_, _, LastL, _), LastL) :-
    format("    ret\n").

emit_im_code(
    return(Expr),
    info(Env, ReturnT, FirstL, _),
    _,
    LastL
) :-
    LastT is ReturnT + 1,

    emit_im_code(
        Expr, info(Env, LastT, FirstL, -1), ReturnT, LastL
    ),
    format("    ret      $t~w\n", [ReturnT]).

emit_im_code(assign(Id, Expr), Info, _, LastL) :-
    Info = info(Env, _, _, _),
    get_assoc(Id, Env, temp(DestT)),
    emit_im_code(Expr, Info, DestT, LastL).

emit_im_code(
    if(Cond, IfTrue, IfFalse),
    info(Env, CondT, IfFalseL, BreakL),
    _,
    LastL
) :-
    EndL is IfFalseL + 1,
    LastL0 is IfFalseL + 2,

    LastT is CondT + 1,
    emit_im_code(
        Cond,
        info(Env, LastT, LastL0, BreakL),
        CondT,
        LastL1
    ),
    format("    bz       $t~w, l~w\n", [CondT, IfFalseL]),
    emit_stmts(
        IfTrue,
        info(Env, LastT, LastL1, BreakL),
        LastL2
    ),
    format("    j        l~w\nl~w:\n", [EndL, IfFalseL]),
    emit_stmts(
        IfFalse,
        info(Env, LastT, LastL2, BreakL),
        LastL
    ),
    format("l~w:\n", [EndL]).

emit_im_code(
    while(Cond, Body),
    info(Env, CondT, StartL, _),
    _,
    LastL
) :-
    CondL is StartL + 1,
    BreakL is StartL + 2,
    LastL0 is StartL + 3,

    %% Reuse "CondT" in the body of the loop, bacause we recompute
    %% the condition on each iteration.
    format("    j        l~w\nl~w:\n", [CondL, StartL]),
    emit_stmts(
        Body,
        info(Env, CondT, LastL0, BreakL),
        LastL1
    ),
    format("l~w:\n", [CondL]),

    LastT is CondT + 1,

    emit_im_code(
        Cond,
        info(Env, LastT, LastL1, -1),
        CondT,
        LastL
    ),
    format(
        "    bnz      $t~w, l~w\nbl~w:\n", [CondT, StartL, BreakL]
    ).

emit_im_code(break, info(_, _, LastL, BreakL), _, LastL) :-
    format("    j        bl~w\n", [BreakL]).

emit_stmts([Statement | Body], Info, LastL) :-
    emit_stmt_and_update_info(Statement, Info, NewInfo),
    emit_stmts(Body, NewInfo, LastL).

emit_stmts([], info(_, _, LastL, _), LastL).

emit_stmt_and_update_info(
    vardef(_, Id),
    info(Env, VarT, FirstL, BreakL),
    info(NewEnv, LastT, FirstL, BreakL)
) :-
    LastT is VarT + 1,

    put_assoc(Id, Env, temp(VarT), NewEnv).

emit_stmt_and_update_info(
    vardef(_, Id, Expr),
    info(Env, VarT, FirstL, BreakL),
    info(NewEnv, LastT, LastL, BreakL)
) :-
    LastT is VarT + 1,

    emit_im_code(
        Expr, info(Env, LastT, FirstL, BreakL), VarT, LastL
    ),
    put_assoc(Id, Env, temp(VarT), NewEnv).

emit_stmt_and_update_info(
    defun(_, Id, Params, Body),
    info(Env, FirstT, FunL, BreakL),
    info(NewEnv, FirstT, LastL, BreakL)
) :-
    LastL0 is FunL + 1,

    put_assoc(Id, Env, label(FunL), NewEnv),
    collect_params(
        Params, info(NewEnv, FirstT), info(FunEnv, LastT)
    ),

    format("l~w:\n", FunL),

    emit_stmts(Body, info(FunEnv, LastT, LastL0, -1), LastL).

emit_stmt_and_update_info(
    Statement,
    Info,
    info(Env, FirstT, LastL, BreakL)
) :-
    Info = info(Env, FirstT, _, BreakL),
    emit_im_code(Statement, Info, 0, LastL).

collect_params(
    [param(_, Id) | Params], info(Env, ParamT), NewInfo
) :-
    LastT is ParamT + 1,
    put_assoc(Id, Env, temp(ParamT), NewEnv),
    collect_params(Params, info(NewEnv, LastT), NewInfo).

collect_params([], Info, Info).

emit_exprlist(
    info(Env, ArgT, FirstL, _),
    LastL,
    [Expr | ExprList],
    [Arg | ArgList]
) :-
    LastT is ArgT + 1,

    emit_im_code(
        Expr,
        info(Env, LastT, FirstL, -1),
        ArgT,
        LastL0
    ),
    emit_exprlist(
        info(Env, LastT, LastL0, -1),
        LastL,
        ExprList,
        ArgList
    ),
    atom_concat('$t', ArgT, Arg).

emit_exprlist(info(_, _, LastL, _), LastL, [], []).
