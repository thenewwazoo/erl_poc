-module(tools).
-export([ lrot/1, rrot/1, lrot/2, rrot/2, rot_tests/0, brol/2, index_of/2 ]).

%% --------------------------------
%%  List rotation helper functions
%% --------------------------------

lrot(L) -> lrot(L, 1).
lrot([], _) -> [];
lrot(L, 0) -> L;
lrot(L, N) when N<0 ->
    rrot(L, N * -1);
lrot(L, N) when N>0 ->
    {Left, Right} = lists:split(N rem length(L), L),
    Right ++ Left.

%% right rotation is just left rotation with a complimentary offset
rrot(L) -> rrot(L, 1).
rrot([], _) -> [];
rrot(L, 0) -> L;
rrot(L, N) when N<0 ->
    lrot(L, N * -1);
rrot(L, N) when N>0 ->
    lrot(L, length(L) - (N rem length(L))).

rot_tests() ->
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

%% --------------------------------
%%  Bit rotation helper functions
%% --------------------------------

brol(Bin,Shift) -> <<U:Shift,Rest/bits>> = Bin, <<Rest/bits,U:Shift>>.

%% --------------------------------
%%  Index search helper
%% --------------------------------
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
