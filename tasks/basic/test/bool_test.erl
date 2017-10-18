-module(bool_test).
-include_lib("eunit/include/eunit.hrl").

bool_test() ->
  Res1 = bool:b_not(true),
  Res2 = bool:b_and(true, true),
  Res3 = bool:b_and(true, false),
  Res4 = bool:b_or(true, false),
  Res5 = bool:b_or(false, false),
  Res6 = bool:b_or(false, true),
  Res7 = bool:b_not(bool:b_or(false, true)),
  Res8 = bool:b_xor(true, false),
  Res9 = bool:b_xor(true, true),

  ?assertNot(Res1),
  ?assert(Res2),
  ?assertNot(Res3),
  ?assert(Res4),
  ?assertNot(Res5),
  ?assert(Res6),
  ?assertNot(Res7),
  ?assert(Res8),
  ?assertNot(Res9).
