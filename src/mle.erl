%% This is a Markov localization engine implementation
-module(mle).

-behaviour(gen_event).

-export([start_link/0,
         add_handler/3]).

-export([init/1, 
         handle_event/2, 
         handle_call/2, 
         handle_info/2, 
         code_change/3, 
         terminate/2]).

-record(mlestate, { whom, 
                    toothprob, 
                    toothdist, 
                    maxaccel, 
                    errorrate,
                    priorstamp,
                    hassync,
                    synctooth,
                    syncconf }).

start_link() ->
    gen_event:start_link().

add_handler(Who, Whom, InitArgs) ->
    Id = {?MODULE, make_ref()},
    Result = gen_event:add_handler(Who, Id, [Whom | InitArgs]),
    {Result, Id}.

init([ Whom, DistMap, MaxAccel, ErrorRate, SyncConf ]) ->
    NewState = #mlestate{ toothprob = normalize([ 1 || _ <- DistMap ]),
                         toothdist = DistMap,
                         maxaccel  = MaxAccel,
                         errorrate = ErrorRate,
                         whom      = Whom,
                         syncconf  = SyncConf,
                         synctooth = 1,
                         hassync   = false,
                         priorstamp = 1 },
    {ok, NewState}.

handle_event({ ecap_capture, { Tw, T1 } }, State) ->
    % Here, we should do our localization
    T0 = State#mlestate.priorstamp,
    Prior = State#mlestate.toothprob,
    Moved = move(State, Prior),
    Located = locate(State, Moved, T1, T0),
    Posterior = normalize(Located),
    Confidence = lists:max(Posterior),
    SyncTooth = tools:index_of(Confidence, Posterior),
    ErrAcc = calc_accel(
                lists:nth( State#mlestate.synctooth, State#mlestate.toothdist ),
                T0,
                lists:nth( SyncTooth, State#mlestate.toothdist ),
                T1,
                lists:sum(State#mlestate.toothdist) 
             ),
    NewState = State#mlestate{ toothprob = Posterior, priorstamp = T1 },
    if
        Confidence >= State#mlestate.syncconf ->
            %% we irrefutably have sync, so we take an early exit
            gen_event:notify( State#mlestate.whom, { sync, { SyncTooth, Tw } } ),
            { ok, NewState#mlestate{ hassync = true, synctooth = SyncTooth } };
        Confidence < State#mlestate.syncconf ->
            %% we could have sync but confidence has dropped due to lots of simple moves
            if
                ErrAcc >= State#mlestate.maxaccel ->
                    gen_event:notify( State#mlestate.whom, { nosync } ),
                    { ok, NewState#mlestate{ hassync = false } };
                ErrAcc < State#mlestate.maxaccel ->
                    { ok, NewState#mlestate{ synctooth = SyncTooth } }
            end
    end;

handle_event({ stall_detected }, State) ->
    % oops! perhaps reset state data?
    % notify subscribers somehow
    {ok, State};

handle_event(Event, State) ->
    R = io_lib:format("~p", [Event]),
    io:format("[~s] got spurious event: ~s~n", [ ?MODULE, lists:flatten(R) ] ),
    {ok, State}.

%% ----------------------------------------------------------------------------
%%  Markov localization functions
%% ----------------------------------------------------------------------------

move(S, Prior) ->
    False = [ P * S#mlestate.errorrate / 2   || P <- Prior ],
    Move  = [ P * (1 - S#mlestate.errorrate) || P <- tools:rrot(Prior, 1) ],
    Miss  = tools:rrot(False, 2),
    Sum = lists:zipwith3( fun(F,M,I) -> F+M+I end, False, Move, Miss ),
    Sum.

locate(S, Moved, Timestamp, PriorTimestamp) ->
    NumToothPosns = lists:sum(S#mlestate.toothdist),
    Located = lists:zipwith3(   fun(D0, D1, P) ->
                                    accel_prob(PriorTimestamp, D0, Timestamp, D1, NumToothPosns, P, S#mlestate.maxaccel, S#mlestate.errorrate)
                                end,
                                tools:rrot(S#mlestate.toothdist, 1),
                                S#mlestate.toothdist,
                                Moved),
    Located.

normalize(L) when is_list(L) ->
    Invsum = 1 / lists:sum(L),
    [ I * Invsum || I <- L ].

%% ----------------------------------------------------------------------------
%%  accel_prob: Calculate the markov probability of a detected transition. In 
%%               other words, calculate the instantaneous acceleration observed,
%%               and return a high or low probability it was observed correctly,
%%               based upon an arbitrary maximum threshold.
%%  - T0: time, in ticks, for the prior event
%%  - D0: distance, in teeth, for the prior event
%%  - T1: time, in ticks, for the current event
%%  - D1: distance, in teeth, for the current event
%%  -  D: total number of tooth "positions" in the sensor wheel
%%  -  P: the posterior probability of the position being considered
%%  -  M: the maximum acceleration above which we consider input to be erroneous
%%  -  E: a tunable parameter indicating how reliable our sensor is
%% ----------------------------------------------------------------------------

accel_prob(T0, D0, T1, D1, D, P, M, E) ->
    Accel = calc_accel(D0, T0, D1, T1, D),
    if
        Accel > M ->
            P * E;
        true ->
            P * (1-E)
    end.

calc_accel(D0, T0, D1, T1, D) ->
    abs( (D1*T0 - D0*T1)/(D*T1*T1*T0) ).

%% ---------------------------------
%% pro forma API calls
%% ---------------------------------

handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
