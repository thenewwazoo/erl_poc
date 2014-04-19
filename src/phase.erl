
%% TODO: This should probably be implemented using gen_fsm...

%% This is an implementation of a phase detection model
-module(phase).
-behaviour(gen_event).

-export([ start_link/0,
          add_handler/3 ]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

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
 
init([]) ->
    {ok, []}.
 
handle_event({ sync, { ToothNum, Timestamp } }, Steve) ->
    % save sync state
    % recalculate current ...
    %  angular velocity
    %  angle
    %  ...more?
    ok.



handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
