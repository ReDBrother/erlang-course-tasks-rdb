-module(gen_db_test).

-include_lib("eunit/include/eunit.hrl").

new_delete_test() ->
  Res1 = gen_db:new(database),
  Res2 = gen_db:new(database),

  ?assert(whereis(database) =/= false),
  ?assertEqual(ok, Res1),
  ?assertMatch({error, _}, Res2),

  ok = gen_db:delete(database),

  ?assertEqual(undefined, whereis(database)).

insert_find_test() ->
  ok = gen_db:new(database1),
  ok = gen_db:new(database2),

  ok = gen_db:insert(database1, key, "value1"),
  ok = gen_db:insert(database2, key, "value2"),

  Res1 = gen_db:find(database1, key),
  Res2 = gen_db:find(database2, key),
  Res3 = gen_db:find(database2, wrong_key),

  ?assertEqual({ok, "value1"}, Res1),
  ?assertEqual({ok, "value2"}, Res2),
  ?assertEqual(not_found, Res3).

delete_objects_test() ->
  ok = gen_db:new(database3),
  ok = gen_db:new(database4),

  ok = gen_db:insert(database3, key1, "value1"),
  ok = gen_db:insert(database4, key1, "value1"),
  ok = gen_db:insert(database3, key2, "value2"),
  ok = gen_db:insert(database4, key2, "value2"),

  ok = gen_db:delete(database3, another_key),
  ok = gen_db:delete(database4, key1),

  Res1 = gen_db:find(database3, key1),
  Res2 = gen_db:find(database4, key1),

  ok = gen_db:delete_all_objects(database3),

  Res3 = gen_db:find(database3, key1),
  Res4 = gen_db:find(database3, key2),
  Res5 = gen_db:find(database4, key2),

  ?assertEqual({ok, "value1"}, Res1),
  ?assertEqual(not_found, Res2),
  ?assertEqual(not_found, Res3),
  ?assertEqual(not_found, Res4),
  ?assertEqual({ok, "value2"}, Res5).

