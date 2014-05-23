%% This is what passes for test code in my sick, sad world.

-module(runit).
-compile(export_all).

hwtest() ->
    {ok, HesMgr, HesPid} = hes:start_link("/sys/devices/platform/ecap-capture.0/capture"),
    {ok, CapMgr} = ecap:start_link(),
    {ok, MleMgr} = mle:start_link(),

    {ok, CapRef} = ecap:add_handler(HesMgr, CapMgr, "/sys/devices/platform/ecap-capture.0/cap_reg"),
    {ok, MleRef} = mle:add_handler(CapMgr, MleMgr, [ [1,1,1,1,2], 1.0e-18, 0.15, 0.945 ] ),

    {ok, TehCapRef} = test_gen_event_handler:add_handler(CapMgr, ["CapMgr"]),
    {ok, TehMleRef} = test_gen_event_handler:add_handler(MleMgr, ["MleMgr"]),
    [ [HesMgr, CapMgr, MleMgr], [CapRef, MleRef], [TehCapRef, TehMleRef], [HesPid] ].

mle_test_start() ->
    {ok, NoneMgr} = gen_event:start(),
    {ok, MleMgr} = gen_event:start({local, mle}),
    gen_event:add_handler(NoneMgr, mle, [MleMgr, [1,1,2], 1.0e-18, 0.15, 0.945]),
    {ok, TehMleRef} = test_gen_event_handler:add_handler(MleMgr, ["MleMgr"]),
    [ NoneMgr, MleMgr, TehMleRef ].

mletest() ->
    [ N, M, T ] = mle_test_start(),
    gen_event:notify(N, this_is_sparta),
    gen_event:notify(N, { ecap_capture, { 100, 100 } }),
    gen_event:notify(N, { ecap_capture, { 200, 100 } }),
    gen_event:notify(N, { ecap_capture, { 400, 200 } }),
    gen_event:notify(N, { ecap_capture, { 500, 100 } }),
    gen_event:notify(N, { ecap_capture, { 600, 100 } }),
    gen_event:notify(N, { ecap_capture, { 800, 200 } }),
    gen_event:notify(N, { ecap_capture, { 900, 100 } }),
    gen_event:notify(N, { ecap_capture, { 1000, 100 } }),
    gen_event:notify(N, { ecap_capture, { 1200, 200 } }),
    gen_event:notify(N, { ecap_capture, { 1300, 100 } }),
    gen_event:stop(N), gen_event:stop(M).
