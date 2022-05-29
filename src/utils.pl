:- module(utils, [is_prefix/3, take_while/4]).

is_prefix([Elem | Prefix], [Elem | List], Rest) :-
    is_prefix(Prefix, List, Rest).

is_prefix([], Rest, Rest).

take_while(Cond, [Elem | List], Rest, [Elem | Consumed]) :-
    call(Cond, Elem), !, take_while(Cond, List, Rest, Consumed).

take_while(_, Rest, Rest, []).
