:- module(intermediate_code, [emit_im_code/2]).

%% Variables with prefix "T" are temporaries, while variables with
%% prefix "L" are labels. Prefix "C" is reserved for code, i.e.,
%% list of intermediate code instructions. I don't like
%% abbreviations, but long lines get annoying after awhile.

%% This predicate takes abstract syntax tree (AST), some info,
%% destination temporary and returns last unused label. Also it takes
%% the beggining and the end of the list that contains intermediate
%% code instruction, effectively using difference lists. Info
%% contains the enviroment for temporaries/labels for
%% variables/functions, first unused temporary, first unused label
%% and break label (for "break" instruction). In short:
%% emit_im_code(Expr, info(Env, FirstT, FirstL, BreakL), DestT, LastL).
%% Temporary 0 is used as garbage temporary (when we want to discard
%% the result), while -1 is used as invalid label.
emit_im_code(Ast, BegC) :-
    list_to_assoc([], Env),
    emit_stmts(Ast, info(Env, 1, 0, -1), code(BegC, []), _).

emit_im_code(expr(Expr), Info, Code, DestT, LastL) :-
    emit_im_code(Expr, Info, Code, DestT, LastL).

emit_im_code(
    inline_if(Cond, IfTrue, IfFalse),
    info(Env, CondT, IfFalseL, _),
    code(BegC, EndC),
    DestT,
    LastL
) :-
    EndL is IfFalseL + 1,
    LastL0 is IfFalseL + 2,
    LastT is CondT + 1,

    emit_im_code(
        Cond,
        info(Env, LastT, LastL0, -1),
        code(BegC, Code0),
        CondT,
        LastL1
    ),
    Code0 = [branch_when_zero(CondT, IfFalseL) | Code1],
    emit_im_code(
        IfTrue,
        info(Env, LastT, LastL1, -1),
        code(Code1, Code2),
        DestT,
        LastL2
    ),
    Code2 = [jump(EndL), label(IfFalseL) | Code3],
    emit_im_code(
        IfFalse,
        info(Env, LastT, LastL2, -1),
        code(Code3, Code4),
        DestT,
        LastL
    ),
    Code4 = [label(EndL) | EndC].

emit_im_code(
    apply(Op, Expr), Info, code(BegC, EndC), DestT, LastL
) :-
    emit_im_code(Expr, Info, code(BegC, Code0), DestT, LastL),
    Code0 = [unary_op(Op, DestT, DestT) | EndC].

emit_im_code(
    apply(Op, Left, Right),
    info(Env, LeftT, FirstL, _),
    code(BegC, EndC),
    DestT,
    LastL
) :-
    LastT is LeftT + 1,
    emit_im_code(
        Left,
        info(Env, LastT, FirstL, -1),
        code(BegC, Code0),
        LeftT,
        LastL0
    ),
    emit_im_code(
        Right,
        info(Env, LastT, LastL0, -1),
        code(Code0, Code1),
        DestT,
        LastL
    ),
    Code1 = [binary_op(Op, DestT, LeftT, DestT) | EndC].

emit_im_code(
    int(Value),
    info(_, _, LastL, _),
    code(BegC, EndC),
    DestT,
    LastL
) :-
    BegC = [movei(DestT, Value) | EndC].

emit_im_code(
    bool(Bool),
    info(_, _, LastL, _),
    code(BegC, EndC),
    DestT,
    LastL) :-
    bool_to_int(Bool, Value),
    BegC = [movei(DestT, Value) | EndC].

emit_im_code(
    funcall(Id, ExprList),
    Info,
    code(BegC, EndC),
    DestT,
    LastL
) :-
    Info = info(Env, _, _, _),
    get_assoc(Id, Env, label(VarL)),
    emit_exprlist(
        Info, code(BegC, Code0), LastL, ExprList, ArgsList
    ),
    Code0 = [call(DestT, ArgsList, VarL) | EndC].

emit_im_code(
    id(Id),
    info(Env, _, LastL, _),
    code(BegC, EndC),
    DestT,
    LastL
) :-
    get_assoc(Id, Env, temp(VarT)),
    BegC = [move(DestT, VarT) | EndC].

emit_im_code(
    return(),
    info(_, _, LastL, _),
    code(BegC, EndC),
    _,
    LastL
) :-
    BegC = [ret() | EndC].

emit_im_code(
    return(Expr),
    info(Env, ReturnT, FirstL, _),
    code(BegC, EndC),
    _,
    LastL
) :-
    LastT is ReturnT + 1,

    emit_im_code(
        Expr,
        info(Env, LastT, FirstL, -1),
        code(BegC, Code0),
        ReturnT,
        LastL
    ),
    Code0 = [ret(ReturnT) | EndC].

emit_im_code(assign(Id, Expr), Info, Code, _, LastL) :-
    Info = info(Env, _, _, _),
    get_assoc(Id, Env, temp(DestT)),
    emit_im_code(Expr, Info, Code, DestT, LastL).

emit_im_code(
    if(Cond, IfTrue, IfFalse),
    info(Env, CondT, IfFalseL, BreakL),
    code(BegC, EndC),
    _,
    LastL
) :-
    EndL is IfFalseL + 1,
    LastL0 is IfFalseL + 2,

    LastT is CondT + 1,
    emit_im_code(
        Cond,
        info(Env, LastT, LastL0, BreakL),
        code(BegC, Code0),
        CondT,
        LastL1
    ),
    Code0 = [branch_when_zero(CondT, IfFalseL) | Code1],
    emit_stmts(
        IfTrue,
        info(Env, LastT, LastL1, BreakL),
        code(Code1, Code2),
        LastL2
    ),
    Code2 = [jump(EndL), label(IfFalseL) | Code3],
    emit_stmts(
        IfFalse,
        info(Env, LastT, LastL2, BreakL),
        code(Code3, Code4),
        LastL
    ),
    Code4 = [label(EndL) | EndC].

emit_im_code(
    while(Cond, Body),
    info(Env, CondT, StartL, _),
    code(BegC, EndC),
    _,
    LastL
) :-
    CondL is StartL + 1,
    BreakL is StartL + 2,
    LastL0 is StartL + 3,

    %% Reuse "CondT" in the body of the loop, bacause we recompute
    %% the condition on each iteration.
    BegC = [jump(CondL), label(StartL) | Code1],
    emit_stmts(
        Body,
        info(Env, CondT, LastL0, BreakL),
        code(Code1, Code2),
        LastL1
    ),
    Code2 = [label(CondL) | Code3],

    LastT is CondT + 1,

    emit_im_code(
        Cond,
        info(Env, LastT, LastL1, -1),
        code(Code3, Code4),
        CondT,
        LastL
    ),
    Code4 = [
        branch_when_not_zero(CondT, StartL), label(BreakL) | EndC
    ].

emit_im_code(
    break,
    info(_, _, LastL, BreakL),
    code(BegC, EndC),
    _,
    LastL
) :-
    BegC = [jump(BreakL) | EndC].

emit_stmts([Statement | Body], Info, code(BegC, EndC), LastL) :-
    emit_stmt_and_update_info(
        Statement, Info, code(BegC, Code0), NewInfo
    ),
    emit_stmts(Body, NewInfo, code(Code0, EndC), LastL).

emit_stmts([], info(_, _, LastL, _), code(BegC, BegC), LastL).

emit_stmt_and_update_info(
    vardef(_, Id),
    info(Env, VarT, FirstL, BreakL),
    code(BegC, BegC),
    info(NewEnv, LastT, FirstL, BreakL)
) :-
    LastT is VarT + 1,

    put_assoc(Id, Env, temp(VarT), NewEnv).

emit_stmt_and_update_info(
    vardef(_, Id, Expr),
    info(Env, VarT, FirstL, BreakL),
    Code,
    info(NewEnv, LastT, LastL, BreakL)
) :-
    LastT is VarT + 1,

    emit_im_code(
        Expr,
        info(Env, LastT, FirstL, BreakL),
        Code,
        VarT,
        LastL
    ),
    put_assoc(Id, Env, temp(VarT), NewEnv).

emit_stmt_and_update_info(
    defun(_, Id, Params, Body),
    info(Env, FirstT, FunL, BreakL),
    code(BegC, EndC),
    info(NewEnv, FirstT, LastL, BreakL)
) :-
    LastL0 is FunL + 1,

    put_assoc(Id, Env, label(FunL), NewEnv),
    collect_params(
        Params, info(NewEnv, FirstT), info(FunEnv, LastT)
    ),

    BegC = [label(FunL) | Code0],

    emit_stmts(
        Body,
        info(FunEnv, LastT, LastL0, -1),
        code(Code0, EndC),
        LastL
    ).

emit_stmt_and_update_info(
    Statement,
    Info,
    Code,
    info(Env, FirstT, LastL, BreakL)
) :-
    Info = info(Env, FirstT, _, BreakL),
    emit_im_code(Statement, Info, Code, 0, LastL).

collect_params(
    [param(_, Id) | Params], info(Env, ParamT), NewInfo
) :-
    LastT is ParamT + 1,
    put_assoc(Id, Env, temp(ParamT), NewEnv),
    collect_params(Params, info(NewEnv, LastT), NewInfo).

collect_params([], Info, Info).

emit_exprlist(
    info(Env, ArgT, FirstL, _),
    code(BegC, EndC),
    LastL,
    [Expr | ExprList],
    [ArgT | ArgList]
) :-
    LastT is ArgT + 1,

    emit_im_code(
        Expr,
        info(Env, LastT, FirstL, -1),
        code(BegC, Code0),
        ArgT,
        LastL0
    ),
    emit_exprlist(
        info(Env, LastT, LastL0, -1),
        code(Code0, EndC),
        LastL,
        ExprList,
        ArgList
    ).

emit_exprlist(info(_, _, LastL, _), code(BegC, BegC), LastL, [], []).

bool_to_int(true, 1).
bool_to_int(false, 0).
