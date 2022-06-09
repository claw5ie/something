:- module(tokenizer, [tokenize/2]).

:- use_module(utils).

tokenize(Source, Tokens) :-
    string_chars(Source, Chars),
    possible_token_strings(TokenStrings),
    tokenize(TokenStrings, Chars, Tokens).

tokenize(TokenStrings, [Char | Chars], Tokens) :-
    is_space(Char),
    tokenize(TokenStrings, Chars, Tokens).

tokenize(TokenStrings, Chars, [Token | Tokens]) :-
    find_token(TokenStrings, Chars, Token, Rest),
    !,
    tokenize(TokenStrings, Rest, Tokens).

tokenize(TokenStrings, [Digit | Chars], [integer(Integer) | Tokens]) :-
    is_digit(Digit),
    take_while(is_digit, Chars, Rest, Digits),
    number_chars(Integer, [Digit | Digits]),
    tokenize(TokenStrings, Rest, Tokens).

tokenize(TokenStrings, [Alpha | Chars], [identifier([Alpha | Alphas]) | Tokens]) :-
    is_alpha(Alpha),
    take_while(tokenizer:is_alnum_or_underscore, Chars, Rest, Alphas),
    tokenize(TokenStrings, Rest, Tokens).

tokenize(_, [], [end_of_file]).

possible_token_strings(
    [
        pair(['r', 'e', 't', 'u', 'r', 'n'], return),
        pair(['b', 'r', 'e', 'a', 'k'], break),
        pair(['w', 'h', 'i', 'l', 'e'], while),
        pair(['e', 'l', 's', 'e'], else),
        pair(['V', 'o', 'i', 'd'], void_type),
        pair(['f', 'u', 'n'], fun),
        pair(['I', 'n', 't'], int_type),
        pair(['i', 'f'], if),
        pair([':', '='], assign),
        pair(['<', '='], less_equal),
        pair(['>', '='], greater_equal),
        pair(['!', '='], different),
        pair(['('], open_paren),
        pair([')'], close_paren),
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

is_alnum_or_underscore(X) :- is_alnum(X).
is_alnum_or_underscore('_').
