-module(list_test).
-include_lib("eunit/include/eunit.hrl").

create_and_create_reverse_test() ->
  Res1 = list:create(10),
  Res2 = list:create_reverse(10),

  ?assertEqual([1,2,3,4,5,6,7,8,9,10], Res1),
  ?assertEqual([10,9,8,7,6,5,4,3,2,1], Res2).

filter_test() ->
  Res = list:filter([1, 2, 3, 4, 5], 3),

  ?assertEqual([1,2,3], Res).

reverse_test() ->
  Res = list:reverse([1,2,3]),

  ?assertEqual([3,2,1], Res).

concatenate_test() ->
  Res = list:concatenate([[1,2,3], [], [4, five]]),

  ?assertEqual([1,2,3,4,five], Res).

flatten_test() ->
  Res = list:flatten([[1,[2,[3],[]],[[[4]]],[5,6]]]),

  ?assertEqual([1,2,3,4,5,6], Res).
