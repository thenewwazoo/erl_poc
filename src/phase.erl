
%% This is an implementation of a phase detection model
-module(phase).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_event).

-export([ start_link/0,
          add_handler/3 ]).

-export([ init/1, 
          handle_event/2, 
          handle_call/2, 
          handle_info/2, 
          code_change/3, 
          terminate/2 ]).

-record(phase, { whom,
                 hassync,
                 synctooth,
                 toothdist,
                 priortime,
                 velocity,
                 angle }).

%% --------------------
%% API
%% --------------------

start_link() ->
    gen_event:start_link().

add_handler(Pid, Handler, Args) ->
    Id = {Handler, make_ref()},
    Result = gen_event:add_handler(Pid, Id, Args),
    {Result, Id}.

%% ---------------------
%% Callbacks
%% ---------------------
 
init([Whom, ToothDist]) ->
    NewState = #phase{ whom = Whom,
                       hassync = false,
                       synctooth = 1,
                       toothdist = ToothDist,
                       priortime = 1,
                       velocity = 1,
                       angle = 0 },
    {ok, NewState}.

handle_event({ sync, { ToothNum, PriorTooth, Clock, Duration } }, State) ->
    % calculate how many teeth we've passed since the last sync message
    D = lists:sum( 
          tools:loopsub( 
            State#phase.toothdist, 
            PriorTooth+1, 
            tools:negmod( ToothNum - PriorTooth, length(State#phase.toothdist) ) ) ),
    % find the number of degrees which that distance represents
    A = D * 360 / lists:sum( State#phase.toothdist ),
    % calculate the velocity in ticks per degree
    V = A / Duration,

    % find the number of tooth positions since TDC, minus one since tooth 1 is at 0°
    TDCDist = lists:sum( lists:sublist( State#phase.toothdist, ToothNum ) ) - 1,
    % find the number of tooth positions total
    TPosns = lists:sum( State#phase.toothdist ),
    % calculate the angle, in degrees, of the last known position
    Theta = tools:fmod(TDCDist * (360 / TPosns), 360),

    % save new data
    {ok, State#phase{ hassync=true,
                      synctooth=ToothNum,
                      priortime=Clock,
                      velocity = V,
                      angle = Theta } };

handle_event({nosync}, State) ->
    {ok, State#phase{ hassync=false } };

handle_event(Event, State) ->
    R = io_lib:format("~p", [Event]),
    io:format("[~s] got spurious event: ~s~n", [ ?MODULE, lists:flatten(R) ] ),
    {ok, State}.

%% Calculate the angle at the given time, Now.
handle_call({get_angle, Now}, State = #phase{ hassync=true }) ->
    io:format("get_angle(~w) called with State = ~w~n", [Now, State]),
    case Now == State#phase.priortime of
      true ->
        {ok, State#phase.angle, State};
      false ->
        Angle = (Now - State#phase.priortime) * State#phase.velocity,
        Theta = State#phase.angle + Angle,
        Thetap = tools:fmod(Theta, 360),
        {ok, Thetap, State}
    end;

%% Calculate the time at which we'll be at the given angle, Theta,
%%  (assuming we can only move forward).
handle_call({get_time, Theta}, State = #phase{ hassync=true }) ->
    io:format("get_time(~w) called with State = ~w~n", [Theta, State]),
    case Theta < State#phase.angle of
      true ->
        handle_call({get_time, Theta + 360}, State);
      false ->
        Displacement = Theta - State#phase.angle,
        Time = (Displacement / State#phase.velocity),
        {ok, Time + State#phase.priortime, State}
    end;

handle_call(_, State = #phase{ hassync = false }) ->
    {ok, nosync, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
