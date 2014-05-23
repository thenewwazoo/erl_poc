%% C node shim for Hall-effect sensor interrupt trigger generators.

-module(hes).

-define(ECAPNODE, 'c1@localhost').

-export([ start_link/1, init/2 ]).

%% ----------------------------------------------------------------------
%%  Function: start_link/1
%%  Purpose: Spawn a process to handle messages from our C node
%%  Args: InterruptSource is a string containing the filename of what we want
%%       the C node to poll(3).
%%  Returns: PIDs of the event manager and the actual handler process
%% ----------------------------------------------------------------------
-spec start_link(list()) -> {atom(), pid(), pid()}.
start_link(InterruptSource) ->
    {ok, EvtMgr} = gen_event:start_link(),
    EvtPid = spawn_link(?MODULE, init, [InterruptSource, EvtMgr]), %% Eventually, we should do real supervision
    {ok, EvtMgr, EvtPid}.

%% ----------------------------------------------------------------------
%%  Function: init/2
%%  Purpose: Initialize the C node and enter the main loop
%%  Args: InterruptSource is the filename of the file the C node will poll(3)
%%        EvtMgr is the event manager to pass messages to
%%  Returns: Does not return, or exits with an error message.
%% ----------------------------------------------------------------------
init(InterruptSource, EvtMgr) ->
    register(evtmgr, EvtMgr),
    {any, ?ECAPNODE} ! { call, self(), { init, InterruptSource } },
    receive
        { ecap_node, ok } ->
            loop();
        Err ->
            io:format("init call to ecap node failed: ~w~n", [Err]),
            error(Err)
    end.

%% ----------------------------------------------------------------------
%%  Function: loop/0 
%%  Purpose: Main loop to handle messages from the C node.
%%  Args: None.
%%  Returns: Does not return.
%% ----------------------------------------------------------------------
loop() ->
    receive
        { data, Data } ->
            %% we're intentionally not doing any manipulation here
            %% we naively pass data, and expect our subscribers to deal with it.
            gen_event:notify(evtmgr, { ding, { Data } }),
            loop()
        %% we also need a way to detect stalling (see mle module's stall_detected event)
    end.
