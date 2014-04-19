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
                    synctooth }).

start_link() ->
    gen_event:start_link().

add_handler(Who, Whom, InitArgs) ->
    Id = {?MODULE, make_ref()},
    Result = gen_event:add_handler(Who, Id, [Whom, InitArgs]),
    {Result, Id}.

init([ Whom, DistMap, MaxAccel, ErrorRate ]) ->
    NewState = #mlestate{ toothprob = normalize([ 1 || _ <- DistMap ]),
                         toothdist = DistMap,
                         maxaccel  = MaxAccel,
                         errorrate = ErrorRate,
                         whom      = Whom },
    {ok, NewState}.

handle_event({ ecap_capture, { Tw, T1 } }, State) ->
    % Here, we should do our localization
    T0 = State#mlestate.priorstamp,
    Prior = State#mlestate.toothprob,
    Moved = move(Prior),
    Located = locate(Moved, T1, T0),
    Posterior = normalize(Located),
    Confidence = lists:max(Posterior),
    SyncTooth = tools:index_of(Confidence, Posterior),
    if
        Confidence >= 0.98 ->
            %% we irrefutably have sync
            gen_event:notify( State#mlestate.whom, { sync, { SyncTooth, Tw } } ),
            { ok, State#mlestate{ toothprob = Posterior, 
                                  hassync = true, 
                                  synctooth = SyncTooth,
                                  priorstamp = T1 };
        Confidence < 0.98 ->
            %% we could have sync but confidence has dropped due to lots of simple moves
            ErrAcc = calc_accel(
                        lists:nth( State#mlestate.synctooth, State#mlestate.toothdist ),
                        T0,
                        lists:nth( SyncTooth, State#mlestate.toothdist ),
                        T1,
                        lists:sum(S#mlestate.toothdist) 
                     ),
            if
                ErrAcc > State#mlestate.maxaccel ->
                    gen_event:notify( State#mlestate.whom, { nosync } ),
                    { ok, State#mlestate{ toothprob = Posterior,
                                          hassync = false,
                                          synctooth = -1,
                                          priorstamp = T1 };
                ErrAcc <= State#mlestate.maxaccel ->
                    { ok, %% return a state here
            end
    end.

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

move(S) when is_record(S, mlestate) ->
    False = [ P * S#mlestate.errorrate / 2   || P <- S#mlestate.toothprob ],
    Move  = [ P * (1 - S#mlestate.errorrate) || P <- listtools:rrot(S#mlestate.toothprob, 1) ],
    Miss  = listtools:rrot(False, 2),
    lists:zipwith3( fun(F,M,I) -> F+M+I end, False, Move, Miss ).

locate(S, Timestamp, PriorTimestamp) when is_record(S, mlestate) ->
    NumToothPosns = lists:sum(S#mlestate.toothdist),
    lists:zipwith3( fun(D0, D1, P) ->
                        accel_prob(PriorTimestamp, D0, Timestamp, D1, NumToothPosns, P, S#mlestate.maxaccel, S#mlestate.errorrate)
                    end,
                    listtools:rrot(S#mlestate.toothdist, 1),
                    S#mlestate.toothdist,
                    S#mlestate.toothprob).

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
