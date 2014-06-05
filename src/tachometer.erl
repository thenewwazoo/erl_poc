
%%% ------------------------------------------------------------------
%%%  This is a very tiny demonstration of how you can build a "subscriber"
%%%   to consume select events from other modules in order to do things.
%%%  This is a tachometer that calculates the current RPM every time the 
%%%   localization engine says we're at a single given location.
%%% ------------------------------------------------------------------


-module(tachometer).

-behaviour(gen_event).

%% API
-export([start_link/0,
         add_handler/2]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {rpm, clockfactor, priortime}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% TimerHz = timer clockspeed in Hz (i.e. 200MHz clock = 200000000)
init([TimerHz]) ->
    { ok, #state{ rpm = 0, clockfactor = TimerHz * 60, priortime = 0 } }.

handle_event({ sync, {1, _, Tw, _} }, State) ->
    case State#state.priortime > Tw of
      true ->
        % Since it's (relatively) rare, we're going to just happily ignore when the timer wraps.
        {ok, State#state{ priortime = Tw }};
      false ->
        % RPM => (1 rev / t ticks) * (hz ticks / 1 sec) * (60 sec / 1 min) = (n rev / 1 min)
        {ok, State#state{ priortime = Tw,
                          rpm = State#state.clockfactor / (Tw - State#state.priortime) } }
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_call({get_rpm}, State) ->
    {ok, State#state.rpm, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
