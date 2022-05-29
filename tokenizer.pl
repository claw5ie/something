:- module(tokenizer, [tokenize/2]).

tokenize(Source, X) :- string_chars(Source, Chars), tokenize_aux(Chars, X).

tokenize_aux([' ' | Rest], X) :-
    tokenize_aux(Rest, X).

tokenize_aux(['i', 'f' | Rest], [if | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['f', 'u', 'n' | Rest], [defun | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['l', 'e', 't' | Rest], [let | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['w', 'h', 'i', 'l', 'e' | Rest], [while | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['b', 'r', 'e', 'a', 'k' | Rest], [break | X]) :-
    !, tokenize_aux(Rest, X).
tokenize_aux(['e', 'l', 's', 'e' | Rest], [else | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['f', 'o', 'r' | Rest], [for | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['r', 'e', 't', 'u', 'r', 'n' | Rest], [return | X]) :-
    tokenize_aux(Rest, X).

tokenize_aux(['V', 'o', 'i', 'd' | Rest], [void_type | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['I', 'n', 't' | Rest], [int_type | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['S', 't', 'r', 'i', 'n', 'g' | Rest], [string_type | X]) :-
    tokenize_aux(Rest, X).

tokenize_aux([':', '=' | Rest], [assign | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['<', '=' | Rest], [less_equal | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['!', '=' | Rest], [different | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['>', '=' | Rest], [greater_equal | X]) :-
    tokenize_aux(Rest, X).

tokenize_aux(['(' | Rest], [lparen | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux([')' | Rest], [rparen | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['[' | Rest], [lbrace | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux([']' | Rest], [rbrace | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux([',' | Rest], [comma | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux([';' | Rest], [semicolon | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux([':' | Rest], [colon | X]) :-
    tokenize_aux(Rest, X).

tokenize_aux(['+' | Rest], [plus | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['-' | Rest], [minus | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['*' | Rest], [multiply | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['/' | Rest], [divide | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['%' | Rest], [modulus | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['=' | Rest], [equal | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['<' | Rest], [less | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['>' | Rest], [greater | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['&' | Rest], [and | X]) :-
    tokenize_aux(Rest, X).
tokenize_aux(['|' | Rest], [or | X]) :-
    tokenize_aux(Rest, X).

tokenize_aux([Digit | Rest], [integer(Int) | X]) :-
    is_digit(Digit),
    take_while(is_digit, Rest, Digits, Other),
    number_chars(Int, [Digit | Digits]),
    tokenize_aux(Other, X).

tokenize_aux([Alpha | Rest], [identifier([Alpha | Alphas]) | X]) :-
    is_alpha(Alpha),
    take_while(is_alnum_or_underscore, Rest, Alphas, Other),
    tokenize_aux(Other, X).

tokenize_aux([], [end_of_file]).

take_while(Cond, [Elem | List], [Elem | Consumed], Rest) :-
    call(Cond, Elem), !, take_while(Cond, List, Consumed, Rest).

take_while(_, Rest, [], Rest).

to_integer(X, Int, Rest) :-
    take_while(is_digit, X, Y, Rest),
    number_chars(Int, Y).

is_alnum_or_underscore(X) :- is_alnum(X).
is_alnum_or_underscore('_').
