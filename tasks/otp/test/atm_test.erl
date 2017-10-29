-module(atm_test).

-include_lib("eunit/include/eunit.hrl").

atm_test() ->
  State = [
    {1, 1234, 5000},
    {2, 4321, 100},
    {3, 2222, 0}
  ],
  Res1 = atm:start_link(State),
  Res2 = atm:push_button('5'),

  Res3 = atm:insert_card(0),
  Res4 = atm:insert_card(1),
  Res5 = atm:insert_card(2),

  ?assertMatch({ok, _}, Res1),
  ?assertMatch({error, _}, Res2),
  ?assertMatch({error, _}, Res3),
  ?assertEqual(ok, Res4),
  ?assertMatch({error,  _}, Res5),

  continue = atm:push_button('0'),
  Res7 = atm:push_button(enter),
  Res8 = atm:insert_card(2),
  atm:push_button(enter),
  Res9 = atm:insert_card(2),
  atm:push_button(enter),
  Res10 = atm:insert_card(2),

  ?assertMatch({error, _}, Res7),
  ?assertMatch({error, _}, Res8),
  ?assertMatch({error, _}, Res9),
  ?assertEqual(ok, Res10),

  atm:terminate(stop).

exchange_test() ->
  State = [
    {1, 1234, 5000},
    {2, 4321, 100},
    {3, 2222, 0}
  ],
  atm:start_link(State),

  ok = atm:insert_card(2),
  Res1 = atm:push_button('cancel'),
  ok = atm:insert_card(2),
  continue = atm:push_button('4'),
  continue = atm:push_button('3'),
  continue = atm:push_button('2'),
  continue = atm:push_button('1'),
  Res2 = atm:push_button(enter),

  ?assertMatch({ok, _}, Res1),
  ?assertMatch({ok, _}, Res2),

  Res3 = atm:push_button(withdraw),
  continue = atm:push_button('2'),
  continue = atm:push_button('0'),
  continue = atm:push_button('0'),
  Res4 = atm:push_button(enter),
  Res5 = atm:push_button(cancel),
  Res6 = atm:push_button(deposit),

  ?assertMatch({ok, _}, Res3),
  ?assertMatch({error, _}, Res4),
  ?assertMatch({ok, _}, Res5),
  ?assertMatch({ok, _}, Res6),

  atm:push_button('1'),
  atm:push_button('0'),
  atm:push_button('0'),
  Res7 = atm:push_button(enter),

  atm:push_button(withdraw),
  atm:push_button('2'),
  atm:push_button('0'),
  atm:push_button('0'),
  Res8 = atm:push_button(enter),
  Res9 = atm:push_button(cancel),

  ?assertMatch({ok, {deposit, 100}}, Res7),
  ?assertMatch({ok, {withdraw, 200}}, Res8),
  ?assertMatch({ok, _}, Res9),

  atm:terminate(stop).
