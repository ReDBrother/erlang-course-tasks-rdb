-module(p_funcs).
-export([pmap/2, pfoldl/3, map/2]).

get_result([], Acc) -> lists:reverse(Acc);
get_result([First|Pids], Acc) ->
  receive
    {First, Result} ->
      get_result(Pids, [Result|Acc])
  end.

pmap(Func, List) ->
  Self = self(),
  SpawnProc = fun(Item) ->
    spawn(fun() ->
      NewItem = Func(Item),
      Self ! {self(), NewItem}
    end)
  end,
  Pids = [SpawnProc(Item) || Item <- List],
  get_result(Pids, []).

pfoldl(Func, Acc, List) ->
  Self = self(),
  SpawnProc = fun(Item) ->
    spawn(fun() ->
      NewItem = Func(Item),
      Self ! {self(), NewItem}
    end)
  end,
  Pids = [SpawnProc(Item) || Item <- List],
  Result = get_result(Pids, []),
  Result ++ Acc.

map(Func, List) -> pfoldl(Func, [], List).
