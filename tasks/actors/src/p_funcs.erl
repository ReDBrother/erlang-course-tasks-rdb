-module(p_funcs).
-export([map/2,
         fold/3,
         fold/4,
         fold/5,
         mapreduce/4,
         mapreduce/5,
         mapreduce/6]).

map(Func, List) ->
  Self = self(),
  SpawnProc = fun(Item) ->
    spawn(fun() ->
      NewItem = Func(Item),
      Self ! {self(), NewItem}
    end)
  end,
  Pids = [SpawnProc(Item) || Item <- List],
  get_result(Pids, []).

fold(Func, InitAcc, List) ->
  fold(Func, Func, InitAcc, List, 5).

fold(Func, InitAcc, List, BatchSize) ->
  fold(Func, Func, InitAcc, List, BatchSize).

fold(Func, Merge_Func, InitAcc, List, BatchSize) ->
  Foldl_Func = fun(BatchList) ->
    lists:foldl(Func, InitAcc, BatchList)
  end,
  reduce_list(Foldl_Func, Merge_Func, InitAcc, List, BatchSize).

mapreduce(Map_Func, Reduce_Func, InitAcc, List) ->
  mapreduce(Map_Func, Reduce_Func, Reduce_Func, InitAcc, List, 5).

mapreduce(Map_Func, Reduce_Func, InitAcc, List, BatchSize) ->
  mapreduce(Map_Func, Reduce_Func, Reduce_Func, InitAcc, List, BatchSize).

mapreduce(Map_Func, Reduce_Func, Merge_Func, InitAcc, List, BatchSize) ->
  MapReduce_Func = fun(BatchList) ->
    lists:foldl(fun(Acc, Item) ->
      Reduce_Func(Acc, Map_Func(Item))
    end, InitAcc, BatchList)
  end,
  reduce_list(MapReduce_Func, Merge_Func, InitAcc, List, BatchSize).

reduce_list(Func, Merge_Func, InitAcc, List, BatchSize) ->
  BatchList = get_batches(BatchSize, BatchSize, List, []),
  Pids = [reduce_proc(Func, Batch) || Batch <- BatchList],
  Result = get_result(Pids, []),
  merge_result(Merge_Func, InitAcc, Result, BatchSize).

get_batches(_, _, [], []) ->
  [];
get_batches(_, _, [], Batch) ->
  [Batch];
get_batches(BatchSize, 0, List, Batch) ->
  [Batch|get_batches(BatchSize, BatchSize, List, [])];
get_batches(BatchSize, N, [Head|Tail], Batch) ->
  get_batches(BatchSize, N - 1, Tail, [Head|Batch]).

reduce_proc(Reduce_Func, List) ->
  Self = self(),
  spawn(fun() ->
    Result = Reduce_Func(List),
    Self ! {self(), Result}
  end).

merge_result(_Func, InitAcc, [], _BatchSize) ->
  InitAcc;
merge_result(_Func, _InitAcc, [Result], _BatchSize) ->
  Result;
merge_result(Func, InitAcc, NewList, BatchSize) ->
  fold(Func, Func, InitAcc, NewList, BatchSize).

get_result([], Acc) -> lists:reverse(Acc);
get_result([First|Pids], Acc) ->
  receive
    {First, Result} ->
      get_result(Pids, [Result|Acc])
  end.
