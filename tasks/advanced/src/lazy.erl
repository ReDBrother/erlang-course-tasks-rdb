-module(lazy).
-export([lazy_map/2,
         lazy_foldl/3,
         lazy_filter/2]).

lazy_map(_, []) ->
  fun() ->
      []
  end;
lazy_map(Func, [Head|Tail]) ->
  fun() ->
      [Func(Head)|lazy_map(Func, Tail)]
  end.

lazy_foldl(_, Acc, []) ->
  fun() ->
      Acc
  end;
lazy_foldl(Func, Acc, [Head|Tail]) ->
  fun() ->
      NewAcc = Func(Head, Acc),
      lazy_foldl(Func, NewAcc, Tail)
  end.

lazy_filter(_, []) ->
  fun() ->
      []
  end;
lazy_filter(Func, [Head|Tail])  ->
  fun() ->
      case Func(Head) of
        true -> [Head|lazy_filter(Func, Tail)];
        false -> lazy_filter(Func, Tail)
      end
  end.
