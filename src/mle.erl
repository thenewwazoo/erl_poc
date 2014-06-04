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
                    minconf,
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

init([ Whom, DistMap, MaxAccel, ErrorRate, MinConf ]) ->
    NewState = #mlestate{ toothprob = normalize([ 1 || _ <- DistMap ]),
                          toothdist = DistMap,
                          maxaccel  = MaxAccel,
                          errorrate = ErrorRate,
                          whom      = Whom,
                          syncconf  = 0.01,
                          minconf   = MinConf,
                          synctooth = 1,
                          hassync   = false,
                          priorstamp = 1 },
    {ok, NewState}.

handle_event({ ecap_capture, { Tw, T1 } }, Prior) ->
    % Do Markov localization with the new data
    Posterior = localize(Prior, T1),
    % Calculate a "sanity check" with our localization. This gives a sort of
    %  hysterisis on our guess, if we've suddenly gotten very confident
    %  but our previous guess was far from here.
    ErrAcc = calc_accel(
                lists:nth( Prior#mlestate.synctooth, Prior#mlestate.toothdist         ), Prior#mlestate.priorstamp,
                lists:nth( Posterior#mlestate.synctooth, Posterior#mlestate.toothdist ), T1,
                lists:sum(Prior#mlestate.toothdist) 
             ),
    case { Posterior, ErrAcc } of
        { S = #mlestate{ maxaccel=MaxAccel }, E } when E >= MaxAccel -> 
            gen_event:notify( S#mlestate.whom, { nosync } ),
            { ok, S#mlestate{ hassync = false } };
        { S = #mlestate{ syncconf=SyncConf, minconf=MinConf }, _ } when SyncConf >= MinConf ->
            gen_event:notify( S#mlestate.whom, { sync, { S#mlestate.synctooth, Prior#mlestate.synctooth, Tw, T1 } } ),
            { ok, S#mlestate{ hassync = true } };
        { S = #mlestate{ hassync=true }, _ } ->
            gen_event:notify( S#mlestate.whom, { sync, { S#mlestate.synctooth, Prior#mlestate.synctooth, Tw, T1 } } ),
            { ok, S };
        { S, _ } ->
            gen_event:notify( S#mlestate.whom, { nosync } ),
            { ok, S }
    end;
            
handle_event({ stall_detected }, State) ->
    gen_event:notify( State#mlestate.whom, { nosync } ),
    {ok, State#mlestate{ hassync = false }};

handle_event(Event, State) ->
    R = io_lib:format("~p", [Event]),
    io:format("[~s] got spurious event: ~s~n", [ ?MODULE, lists:flatten(R) ] ),
    {ok, State}.

%% ----------------------------------------------------------------------------
%%  Markov localization functions
%% ----------------------------------------------------------------------------

localize(Prior, T1) ->
    Moved = move(Prior),
    Located = locate(Moved, T1),
    Normalized = normalize(Located),
    Confidence = lists:max(Normalized#mlestate.toothprob),
    Normalized#mlestate{ syncconf   = Confidence,
                         priorstamp = T1,
                         synctooth  = tools:index_of(Confidence, Normalized#mlestate.toothprob) }.

%% move(#mlestate{ toothprob = P, errorrate = MaxErr }, S) ->
move(S) ->
    False = [ P * S#mlestate.errorrate / 2   || P <- S#mlestate.toothprob ],
    Move  = [ P * (1 - S#mlestate.errorrate) || P <- tools:rrot(S#mlestate.toothprob, 1) ],
    Miss  = tools:rrot(False, 2),
    Sum = lists:zipwith3( fun(F,M,I) -> F+M+I end, False, Move, Miss ),
    S#mlestate{ toothprob = Sum }.

locate(S, T1) ->
    NumToothPosns = lists:sum(S#mlestate.toothdist),
    Located = lists:zipwith3( fun(D0, D1, P) ->
                                  accel_prob(S#mlestate.priorstamp, D0,     % prior
                                             T1, D1,                        % current
                                             NumToothPosns, P,
                                             S#mlestate.maxaccel, S#mlestate.errorrate)
                              end,
                              tools:rrot(S#mlestate.toothdist, 1),
                              S#mlestate.toothdist,
                              S#mlestate.toothprob),
    S#mlestate{ toothprob = Located }.

normalize(S) when is_record(S, mlestate) ->
    S#mlestate{ toothprob = normalize(S#mlestate.toothprob) };
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
