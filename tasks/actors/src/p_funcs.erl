-module(p_funcs).
-export([pmap/2, pfoldl/3, map_reduce/4]).

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

foldl_proc(Func, MFunc, InitAcc, Batch) ->
  Self = self(),
  spawn(fun() ->
    Result = case Func == MFunc of
      true ->
        lists:foldl(MFunc, InitAcc, Batch);
      false ->
        lists:foldl(fun(X, Acc) ->
          MFunc(Func(X), Acc) end, InitAcc, Batch)
    end,
    Self ! {self(), Result}
  end).

get_batches(_, _, [], Batch) ->
  [Batch];
get_batches(BatchSize, 0, List, Batch) ->
  [Batch|get_batches(BatchSize, BatchSize, List, [])];
get_batches(BatchSize, N, [Head|Tail], Batch) ->
  get_batches(BatchSize, N - 1, Tail, [Head|Batch]).

pfoldl(MFunc, InitAcc, List) ->
  pfoldl(MFunc, MFunc, 5, InitAcc, List).

pfoldl(Func, MFunc, BatchSize, InitAcc, List) ->
  BatchList = get_batches(BatchSize, BatchSize, List, []),
  Pids = [foldl_proc(Func, MFunc, InitAcc, Batch) || Batch <- BatchList],
  ResultList = get_result(Pids, []),
  case ResultList of
    [Result] ->
      Result;
    NewList ->
      pfoldl(MFunc, MFunc, BatchSize, InitAcc, NewList)
  end.

map_reduce(Func, MFunc, InitAcc, List) ->
  pfoldl(Func, MFunc, 5, InitAcc, List).

