:- module(tokenizer, [tokenize/2]).

:- use_module(utils).

tokenize([Char | Chars], Tokens) :-
    is_space(Char),
    tokenize(Chars, Tokens).

tokenize(['/', '/' | Chars], Tokens) :-
    !,
    consume_comment(Chars, Rest),
    tokenize(Rest, Tokens).

tokenize(Chars, [Token | Tokens]) :-
    possible_token_strings(TokenStrings),
    find_token(TokenStrings, Chars, Token, Rest),
    !,
    tokenize(Rest, Tokens).

tokenize([Digit | Chars], [int(Integer) | Tokens]) :-
    is_digit(Digit),
    take_while(is_digit, Chars, Rest, Digits),
    number_chars(Integer, [Digit | Digits]),
    tokenize(Rest, Tokens).

tokenize([Alpha | Chars], [id([Alpha | Alphas]) | Tokens]) :-
    is_alpha(Alpha),
    take_while(tokenizer:is_valid_id_char, Chars, Rest, Alphas),
    tokenize(Rest, Tokens).

tokenize([], []).

possible_token_strings(
    [
        pair(['r', 'e', 't', 'u', 'r', 'n'], return),
        pair(['b', 'r', 'e', 'a', 'k'], break),
        pair(['f', 'a', 'l', 's', 'e'], bool(false)),
        pair(['w', 'h', 'i', 'l', 'e'], while),
        pair(['B', 'o', 'o', 'l'], bool_type),
        pair(['e', 'l', 's', 'e'], else),
        pair(['t', 'r', 'u', 'e'], bool(true)),
        pair(['V', 'o', 'i', 'd'], void_type),
        pair(['f', 'u', 'n'], fun),
        pair(['I', 'n', 't'], int_type),
        pair(['i', 'f'], if),
        pair([':', '='], assign),
        pair(['<', '='], less_equal),
        pair(['>', '='], greater_equal),
        pair(['!', '='], different),
        pair(['('], open_paren),
        pair([')'], closed_paren),
        pair(['{'], open_curly),
        pair(['}'], closed_curly),
        pair([','], comma),
        pair([':'], colon),
        pair([';'], semicolon),
        pair(['|'], or),
        pair(['&'], and),
        pair(['='], equal),
        pair(['<'], less),
        pair(['>'], greater),
        pair(['+'], plus),
        pair(['-'], minus),
        pair(['*'], multiply),
        pair(['/'], divide),
        pair(['%'], modulus)
    ]
).

find_token([pair(TokenString, Token) | _], String, Token, Rest) :-
    is_prefix(TokenString, String, Rest).

find_token([_ | TokenStrings], String, Token, Rest) :-
    find_token(TokenStrings, String, Token, Rest).

consume_comment([Char | Chars], Rest) :-
    Char \= '\n',
    consume_comment(Chars, Rest).

consume_comment(['\n' | Chars], Chars).

consume_comment([], []).

is_valid_id_char(X) :- is_alnum(X).
is_valid_id_char('_').
