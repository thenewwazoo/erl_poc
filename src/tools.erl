-module(tools).
-export([ lrot/1, rrot/1, lrot/2, rrot/2, brol/2, index_of/2, negmod/2, fmod/2, loopsub/3 ]).
-export([ rot_test/0, negmod_test/0 ]).

%%% --------------------------------
%%%  List rotation helper functions
%%% --------------------------------

lrot(L) -> lrot(L, 1).
lrot([], _) -> [];
lrot(L, 0) -> L;
lrot(L, N) when N<0 ->
    rrot(L, N * -1);
lrot(L, N) when N>0 ->
    {Left, Right} = lists:split(N rem length(L), L),
    Right ++ Left.

%%% right rotation is just left rotation with a complimentary offset
rrot(L) -> rrot(L, 1).
rrot([], _) -> [];
rrot(L, 0) -> L;
rrot(L, N) when N<0 ->
    lrot(L, N * -1);
rrot(L, N) when N>0 ->
    lrot(L, length(L) - (N rem length(L))).

rot_test() ->
    Testlist = [1,2,3,4,5],
    Lrotlist = [3,4,5,1,2],
    Rrotlist = [4,5,1,2,3],
    true = Lrotlist == lrot(Testlist, 2),
    true = Lrotlist == rrot(Testlist, -2),
    true = Lrotlist == lrot(Testlist, 7),
    true = Lrotlist == rrot(Testlist, -7),
    true = Rrotlist == rrot(Testlist, 2),
    true = Rrotlist == lrot(Testlist, -2),
    true = Rrotlist == rrot(Testlist, 7),
    true = Rrotlist == lrot(Testlist, -7),
    true = Testlist == lrot( rrot(Testlist, 2), 2),
    ok.

%%% --------------------------------
%%%  Bit rotation helper functions
%%% --------------------------------

brol(Bin,Shift) -> <<U:Shift,Rest/bits>> = Bin, <<Rest/bits,U:Shift>>.

%%% --------------------------------
%%%  Index search helper
%%% --------------------------------
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

%%% --------------------------------
%%%  Negative modulo helper (default behaviour is weird)
%%% --------------------------------

negmod(X,Y) -> (X rem Y + Y) rem Y.

negmod_test() -> 1 = negmod(-4, 5).

%%% --------------------------------
%%%  Floating point "remainder" helper
%%% --------------------------------

fmod(X, Y) -> 
    Div = float(trunc(X/Y)),
    Rem = X - Div*Y,
    Rem.

%%% --------------------------------
%%%  Looping sublist helper
%%%   This function assumes that the inputs are sane, namely:
%%%      I+N-length(L) < length(L)
%%%      I < length(L)
%%% --------------------------------

loopsub(L, I, N) when I+N > length(L) ->
    lists:sublist(L, I, N) ++ lists:sublist(L, 1, I + N - length(L) - 1);
loopsub(L, I, N) ->
    lists:sublist(L, I, N).
