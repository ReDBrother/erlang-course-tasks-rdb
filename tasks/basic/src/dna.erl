-module(dna).
-export([cut_rdna/2, rdna/1]).

rdna([]) -> "";
rdna([Head|Tail]) ->
 change(Head) ++ rdna(Tail).

change(Item) ->
  case Item of
    71 -> "C";
    g -> "C";
    67 -> "G";
    c -> "G";
    84 -> "A";
    t -> "A";
    65 -> "U";
    a -> "U"
  end.

cut_rdna(Item, Match) when length(Item) < length(Match) ->
  Item;
cut_rdna([Head|Tail] = Item, Match) ->
  case starts_with(Item, Match) of
    true ->
      cut_rdna(cut(Item, length(Match)), Match);
    false ->
      [Head] ++ cut_rdna(Tail, Match)
  end.

starts_with(_, []) -> true;
starts_with([Head1|Tail1], [Head2|Tail2]) ->
  if
    Head1 =:= Head2 -> starts_with(Tail1, Tail2);
    true -> false
  end.

cut(Item, N) when N < 1 -> Item;
cut([_|Tail], N) ->
  cut(Tail, N - 1).

