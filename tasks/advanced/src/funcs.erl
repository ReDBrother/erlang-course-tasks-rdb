-module(funcs).
-export([get_calc_merge_func/0]).

get_calc_merge_func() ->
  fun
    CalcMerge(Operation, [X1|Tail1], [X2|Tail2]) ->
      [Operation(X1, X2)|CalcMerge(Operation, Tail1, Tail2)];
    CalcMerge(_, [], []) ->
      []
  end.


