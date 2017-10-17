-module(my_lists).
-export([calc_merge/3, calc_dist/3, filtermap/2]).

calc_merge(_, _, _) -> ok.

calc_dist(_, _, _) -> ok.

filtermap(Func, List) ->
  lists:foldl(fun(Elem, Acc) ->
      case Func(Elem) of
        true -> Acc ++ [Elem];
        false -> Acc;
        {true, Value} -> Acc ++ [Value]
      end
    end, [], List).
