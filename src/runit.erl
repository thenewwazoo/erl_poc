%% This is what passes for test code in my sick, sad world.

-module(runit).
-compile(export_all).

dotest() ->
    {ok, HesMgr, HesPid} = hes:start_link("/sys/devices/platform/ecap-capture.0/capture"),
    {ok, CapMgr} = ecap:start_link(),
    {ok, MleMgr} = mle:start_link(),

    {ok, CapRef} = ecap:add_handler(HesMgr, CapMgr, "/sys/devices/platform/ecap-capture.0/cap_reg"),
    {ok, MleRef} = mle:add_handler(CapMgr, MleMgr, [ [1,1,1,1,2], 1.0e-18, 0.15, 0.945 ] ),

    {ok, TehCapRef} = test_gen_event_handler:add_handler(CapMgr, ["CapMgr"]),
    {ok, TehMleRef} = test_gen_event_handler:add_handler(MleMgr, ["MleMgr"]),
    [ [HesMgr, CapMgr, MleMgr], [CapRef, MleRef], [TehCapRef, TehMleRef], [HesPid] ].
