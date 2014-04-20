-module(runit).
-compile(export_all).

dotest() ->
    {ok, HesMgr} = hes:start_link("/sys/devices/platform/ecap-capture.0/capture"),
    {ok, CapMgr} = ecap:start_link(),
    {ok, CapRef} = ecap:add_handler(HesMgr, CapMgr, "/sys/devices/platform/ecap-capture.0/cap_reg"),
    {ok, TehHesRef} = test_gen_event_handler:add_handler(HesMgr, ["HesMgr"]),
    {ok, TehCapRef} = test_gen_event_handler:add_handler(CapMgr, ["CapMgr"]),
    [ HesMgr, CapMgr, CapRef, [TehHesRef, TehCapRef] ].
