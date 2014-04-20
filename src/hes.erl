%% Driver shim for Hall-effect sensor interrupt triggers.
-module(hes).

-define(ECAPNODE, 'c1@localhost').

-export([ start_link/1, init/2 ]).

start_link(InterruptSource) ->
    {ok, EvtMgr} = gen_event:start_link(),
    spawn_link(?MODULE, init, [InterruptSource, EvtMgr]), %% Eventually, we should do real supervision
    {ok, EvtMgr}.

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

loop() ->
    receive
        { data, Data } ->
            %% we're intentionally not doing any manipulation here
            %% we naively pass data, and expect our subscribers to deal with it.
            gen_event:notify(evtmgr, { ding, { Data } }),
            loop()
        %% we also need a way to detect stalling (see mle module's stall_detected event)
    end.
