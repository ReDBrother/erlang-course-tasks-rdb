-module(db_test).
-include_lib("eunit/include/eunit.hrl").

db_read_write_delete_test() ->
  Db = db:new(),
  Res = db:read(two, Db),
  ?assertMatch({error, instance}, Res),

  Db1 = db:write(one, 123, Db),
  {ok, Res1} = db:read(one, Db1),
  ?assertEqual(123, Res1),

  Db2 = db:write(two, "foo", Db1),
  Db3 = db:delete(one, Db2),
  {ok, Res2} = db:read(two, Db3),
  Res3 = db:read(one, Db3),
  ?assertEqual("foo", Res2),
  ?assertMatch({error, instance}, Res3).

db_read_delete_batch_test() ->
  Db = db:new([{batch, 2}]),
  Res = db:batch_read([one], Db),
  ?assertMatch({error, instance}, Res),

  Db1 = db:write(one, "foo", Db),
  Db2 = db:write(two, "bar", Db1),
  Db3 = db:write(three, 123, Db2),

  Res1 = db:batch_read([one, two, three], Db3),
  Res2 = db:batch_read([one, four], Db3),
  Res3 = db:batch_read([one, two], Db3),
  ?assertMatch({error, batch_limit}, Res1),
  ?assertMatch({error, instance}, Res2),
  ?assertEqual([{one, "foo"}, {two, "bar"}], Res3),

  Res4 = db:batch_delete([one, two, three], Db3),
  ?assertMatch({error, batch_limit}, Res4),

  Db4 = db:batch_delete([one, two], Db3),

  Res5 = db:read(one, Db4),
  Res6 = db:read(two, Db4),
  {ok, Res7} = db:read(three, Db4),
  ?assertMatch({error, instance}, Res5),
  ?assertMatch({error, instance}, Res6),
  ?assertEqual(123, Res7).
