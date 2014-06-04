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

tacho_test_start(Hz, Td, MaxAcc, ErrRate, MinConf) ->
    [ NoneMgr, MleMgr ] = mle_test_start(Td, MaxAcc, ErrRate, MinConf),
    {ok, Tacho} = gen_event:start({local, tachometer}),
    ok = gen_event:add_handler(MleMgr, tachometer, [Hz]),
    [ NoneMgr, MleMgr, Tacho ].

phase_test_start(Td, MaxAcc, ErrRate, MinConf) ->
    [ NoneMgr, MleMgr ] = mle_test_start(Td, MaxAcc, ErrRate, MinConf),
    {ok, PhaseMgr} = gen_event:start({local, phase}),
    ok = gen_event:add_handler(MleMgr, phase, [PhaseMgr, Td]),
    [ NoneMgr, MleMgr, PhaseMgr ].

mle_test_start(Td, MaxAcc, ErrRate, MinConf) ->
    {ok, NoneMgr} = gen_event:start(),
    {ok, MleMgr} = gen_event:start({local, mle}),
    ok = gen_event:add_handler(NoneMgr, mle, [MleMgr, Td, MaxAcc, ErrRate, MinConf]),
    [ NoneMgr, MleMgr ].

phase_test_stop(N, M, P) ->
    ok = gen_event:stop(P),
    ok = mle_test_stop(N, M),
    ok.

mle_test_stop(N, M) ->
    ok = gen_event:stop(N), gen_event:stop(M),
    ok.

tachotest() ->
    [ Td, MaxAcc, ErrRate, MinConf] = seedvalues(),
    [ N, M, T ] = tacho_test_start(100, Td, MaxAcc, ErrRate, MinConf),
    {ok, TehMleRef} = test_gen_event_handler:add_handler(M, ["MleMgr"]),
    stimulate(N),
    timer:sleep(10),
    Rpm = gen_event:call(M, tachometer, {get_rpm}),
    io:format("RPM is ~w~n", [Rpm]),
    ok = phase_test_stop(N, M, T),
    ok.

phasetest() ->
    [ Td, MaxAcc, ErrRate, MinConf ] = seedvalues(),
    [ N, M, P ] = phase_test_start(Td, MaxAcc, ErrRate, MinConf),
    {ok, TehMleRef} = test_gen_event_handler:add_handler(M, ["MleMgr"]),
    {ok, TehPhsRef} = test_gen_event_handler:add_handler(P, ["PhaseMgr"]),
    irritate(N), irritate(M), irritate(P),
    stimulate(N),
    timer:sleep(10),
    io:format("~s", [io_lib:format("get_angle(1300) returned: ~8.1f~n", [gen_event:call(M, phase, {get_angle, 1300})])]),
    io:format("~s", [io_lib:format("get_angle(1400) returned: ~8.1f~n", [gen_event:call(M, phase, {get_angle, 1400})])]),
    io:format("~s", [io_lib:format("get_time(0) returned: ~8.1f~n", [gen_event:call(M, phase, {get_time, 0})])]),
    io:format("~s", [io_lib:format("get_time(90) returned: ~8.1f~n", [gen_event:call(M, phase, {get_time, 90})])]),
    ok = phase_test_stop(N, M, P),
    ok.

mletest() ->
    [ Td, MaxAcc, ErrRate, MinConf ] = seedvalues(),
    [ N, M ] = mle_test_start(Td, MaxAcc, ErrRate, MinConf ),
    {ok, TehMleRef} = test_gen_event_handler:add_handler(M, ["MleMgr"]),
    irritate(N),
    stimulate(N),
    {ok} = mle_test_stop(N, M),
    ok.

seedvalues() ->
    Td = [1,1,2],
    MaxAcc = 1.0e-18,
    ErrRate = 0.15,
    MinConf = 0.945,
    [ Td, MaxAcc, ErrRate, MinConf ].

irritate(N) ->
    gen_event:notify(N, this_is_sparta).

stimulate(N) ->
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
    {ok}.
