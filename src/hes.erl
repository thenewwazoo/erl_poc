%% Driver shim for Hall-effect sensor interrupt triggers.
-module(hes).

-export([ start_link/1, init/2 ]).

%% This file name must not include the .so extension, and must match the
%%  ErlDrvEntry.driver_name struct field in the driver, or else erl_ddll:load_driver/2
%%  will return {error, bad_driver_name}.
-define(INTDRV, "interrupt_drv").

start_link(InterruptSource) ->
    {ok, EvtMgr} = gen_event:start_link(),
    spawn_link(?MODULE, init, [InterruptSource, EvtMgr]), %% Eventually, we should do real supervision
    {ok, EvtMgr}.

init(InterruptSource, EvtMgr) ->
    register(evtmgr, EvtMgr),
    {any, 'ecap@localhost'} ! { call, self(), InterruptSource },
    receive
        { ecap_node, ok } ->
            loop();
        Err ->
            io:format("init call to ecap node failed: ~w~n", Err),
            error(Err)
    end.

loop() ->
    receive
        { Port, {data, Data} } when Port == IntPort ->
            %% we're intentionally not doing any manipulation here
            %% we naively pass data, and expect our subscribers to deal with it.
            gen_event:notify(evtmgr, { ding, { Data } }),
            loop()
        %% we also need a way to detect stalling (see mle module's stall_detected event)
    end.
