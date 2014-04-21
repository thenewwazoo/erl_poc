-module(ecap).

-behaviour(gen_event).

%% API
-export([
          start_link/0
        , add_handler/3
        ]).

%% gen_event callbacks
-export([
          init/1
        , handle_event/2
        , handle_call/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {whom=undefined, capregfile=undefined}).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
    gen_event:start_link().

%% perhaps this function should be called "notify_thismgr(Mgr, [Args,To,?MODULE])"
%% and perhaps should be add_notifier(Who, Whom, Args) -> add_handler(Who, [Whom | Args]).
add_handler(Who, Whom, CapRegFile) ->
    Id = {?MODULE, make_ref()},
    Result = gen_event:add_handler(Who, Id, [Whom,CapRegFile]),
    {Result, Id}.

%% ===================================================================
%%  gen_event callbacks
%% ===================================================================

init([ Whom, CapRegFile ]) ->
    NewState = #state{whom=Whom, capregfile=CapRegFile},
    {ok, NewState}.

handle_event({ ding, { Data } }, State) ->
    %% Data is ecap_regs->ECFLG
    EcapFlg = list_to_integer(Data, 16),
    {ok, CapStr} = file:read_file(State#state.capregfile),
    CapTok = string:tokens(binary_to_list(CapStr), ","),
    Stamps = [ list_to_integer(T, 16) || T <- CapTok ],
    [ gen_event:notify(State#state.whom, { ecap_capture, { Tw, T1 } }) || { Tw, T1 } <- get_tstamp(Stamps, EcapFlg) ],
    {ok, State};

handle_event(Event, State) ->
    R = io_lib:format("~p", [Event]),
    io:format("[~s] got spurious event: ~s~n", [ ?MODULE, lists:flatten(R) ] ),
    {ok, State}.

%% ===================================================================
%%  Internal functions
%% ===================================================================

%% CapReg = [ CAP4, CAP3, CAP2, CAP1 ]
%% EcapFLG = ecap_regs->ECFLG => bitfield { .., .cap4, .cap3, .cap2, .cap1, ... }
get_tstamp( CapReg, EcapFlg ) ->

    %% Unpack ECFLG
    <<_Other:2, _OVFBit:1, FourBit:1, ThreeBit:1, TwoBit:1, OneBit:1, _GenBit:1>> = <<EcapFlg:8>>,

    %% Here we find the "lowest" state of the flag input. This state gives us
    %%  a resultant list ordered by age, from newest flag to oldest flag. The reason
    %%  for this is to handle situations where CAPn is newer than CAP(n+m) because the
    %%  registers have "wrapped" in time. This finds the "break" (zeroes) in the flag
    %%  register via rotation. It's basically only for the (hopefully unusual) edge
    %%  case when we've missed a(n) interrupt(s).
    {Flagbits, R} = find_lowest_rotation([ FourBit, ThreeBit, TwoBit, OneBit ]),
    Timerlist = tools:lrot( CapReg, R ),

    %% Calculate time deltas between captures using two's compliment unsigned subtraction
    Timediffs = lists:zipwith( fun(New, Old) -> binary:decode_unsigned(<<(New - Old)>>) end, Timerlist, tools:lrot(Timerlist, 1)),

    Now     = bits_filter(Flagbits, Timerlist),
    Current = bits_filter(Flagbits, Timediffs),
    %% We do some shifting here to make the values line up with the Current list
    %%Prior   = tools:lrot(bits_filter(tools:rrot(Flagbits), Timediffs)),

    lists:zip( Now, Current ).

bits_filter(Bits, Stamps) ->
    Filtered = lists:filter( fun({Bit, _Stamp}) -> if Bit == 1 -> true; Bit /= 1 -> false end end, lists:zip(Bits, Stamps) ),
    [ S || {_B, S} <- Filtered ].

find_lowest_rotation(L) -> find_lowest_rotation(L, 0).
find_lowest_rotation(L, N) when N < length(L) ->
    Rot = tools:lrot(L),
    if
        Rot < L ->
            find_lowest_rotation(Rot, N+1);
        Rot >= L ->
            {L, N}
    end.

%% ===================================================================
%%  pro-forma definitions
%% ===================================================================
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
