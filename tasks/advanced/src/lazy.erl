-module(lazy).
-export([map/2,
         foldl/3,
         filter/2,
         concatenate/1,
         read_file/1]).

map(_, []) ->
  fun() ->
    []
  end;
map(Func, [Head|Tail]) ->
  fun() ->
    [Func(Head)|map(Func, Tail)]
  end.

foldl(_, Acc, []) ->
  fun() ->
    Acc
  end;
foldl(Func, Acc, [Head|Tail]) ->
  fun() ->
    NewAcc = Func(Head, Acc),
    foldl(Func, NewAcc, Tail)
  end.

filter(_, []) ->
  fun() ->
    []
  end;
filter(Func, [Head|Tail])  ->
  fun() ->
      case Func(Head) of
        true -> [Head|filter(Func, Tail)];
        false -> (filter(Func, Tail))()
      end
  end.

append([], LazyList) ->
  LazyList;
append([Head|Tail], LazyList) ->
  fun() ->
    [Head|append(Tail, LazyList)]
  end.

concatenate([]) ->
  fun() ->
    []
  end;
concatenate([Head|Tail]) ->
  append(Head, concatenate(Tail)).

read_file(File) ->
  case file:open(File, read) of
    {error, Reason} ->
      {error, Reason};
    {ok, F} ->
      fun () ->
          read_line(F, "")
      end
  end.

read_line(F, "") ->
  case file:read(F, 1) of
    {ok, "\n"} ->
        "";
    eof ->
      eof;
    {ok, Item} ->
      read_line(F, Item)
  end;
read_line(F, Acc) ->
  case file:read(F, 1) of
    {ok, "\n"} ->
      Acc;
    eof ->
      Acc;
    {ok, Item} ->
      read_line(F, Acc ++ Item)
  end.

