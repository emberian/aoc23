
isok([], []).

% Handle a block of 'on' cells. 'V' is the value (length of the contiguous 'on' block),
% and 'Rest' is the rest of the values list.
isok([on | R], [V | Vs]) :-
    match_on([on | R], V, RestR),  % Match the remaining 'on' cells in the block.
    isok(RestR, Vs).  % Continue with the rest of the row and values.
isok([off | R], Vs) :-
    isok(R, Vs).  % Simply skip 'off' cells and continue.
%isok(_ : R, V) :- isok(on : R, V) ; isok(off : R, V).  % Try both 'on' and 'off'.

restrok([]).
restrok([off | _]).

match_on(R, 0, R).
match_on(R, Ct, RestR) :-
    length(Ons, Ct),
    prefix(Ons, R),
    nth0(_, Ons, on),
    append(Ons, RestR, R),
    restrok(RestR).