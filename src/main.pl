:- module(main, [compile_source_code/1]).

:- use_module(tokenizer).
:- use_module(parser).
:- use_module(typechecker).
:- use_module(intermediate_code).

compile_source_code(File) :-
    open(File, read, In),
    read_string(In, _, SourceCode),

    tokenize(SourceCode, Tokens),
    write(Tokens),
    write("\n\n"),

    parse(Tokens, Ast),
    write(Ast),
    write("\n\n"),

    typecheck(Ast),

    emit_im_code(Ast).
