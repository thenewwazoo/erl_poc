-module(test_gen_event_handler).

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

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(EvtMgr, Args) ->
    Id = {?MODULE, make_ref()},
    Result = gen_event:add_handler(EvtMgr, Id, Args),
    {Result, Id}.

init([Name]) ->
    {ok, Name}.

handle_event(Data, State) ->
    R = io_lib:format("~p", [Data]),
    io:format("TEH [~s] got Data: ~s~n", [ State, lists:flatten(R) ] ),
    {ok, State}.

%% nothing interesting below

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
