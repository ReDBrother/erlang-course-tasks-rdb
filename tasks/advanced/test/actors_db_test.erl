-module(actors_db_test).
-include_lib("eunit/include/eunit.hrl").

actors_db_read_write_test() ->
  Pid = actors_db:new(),

  Res1 = actors_db:write(one, 123, Pid),
  Res2 = actors_db:read(one, Pid),
  Res3 = actors_db:write(two, "foo", Pid),
  Res4 = actors_db:read(two, Pid),
  Res5 = actors_db:read("foo", Pid),

  ?assertMatch(ok, Res1),
  ?assertEqual(123, Res2),
  ?assertMatch(ok, Res3),
  ?assertEqual("foo", Res4),
  ?assertMatch({error, instance}, Res5).

actors_db_match_delete_test() ->
  Pid = actors_db:new(),

  actors_db:write(one, 123, Pid),
  actors_db:write(two, 123, Pid),

  Res1 = actors_db:match(123, Pid),
  Res2 = actors_db:match(1234, Pid),
  Res3 = actors_db:delete(two, Pid),
  Res4 = actors_db:delete(three, Pid),
  Res5 = actors_db:read(two, Pid),
  Res6 = actors_db:match(123, Pid),

  ?assertEqual([two, one], Res1),
  ?assertEqual([], Res2),
  ?assertMatch(ok, Res3),
  ?assertMatch(ok, Res4),
  ?assertMatch({error, instance}, Res5),
  ?assertEqual([one], Res6).
