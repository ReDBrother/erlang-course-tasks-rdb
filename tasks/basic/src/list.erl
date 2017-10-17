-module(list).
-export([create/1,
         create_reverse/1,
         print_numbs/1,
         print_even_numbs/1,
         filter/2,
         reverse/1,
         concatenate/1,
         flatten/1]).

create(N) when N < 1 -> [];
create(N) -> create(N, 1).

create(N, N) -> [N];
create(N, Item) ->
  Tail = create(N, Item + 1),
  [Item|Tail].

create_reverse(N) when N < 1 -> [];
create_reverse(N) -> [N|create_reverse(N - 1)].

print_numbs(Num, N) when Num > N -> ok;
print_numbs(Num, N) ->
  io:fwrite("~w~n", [Num]),
  print_numbs(Num + 1, N).

print_numbs(N) when N < 1 -> ok;
print_numbs(N) -> print_numbs(1, N).

print_even_numbs(Num, N) when Num > N -> ok;
print_even_numbs(Num, N) ->
  io:fwrite("~w~n", [Num]),
  print_even_numbs(Num + 2, N).

print_even_numbs(N) ->
  print_even_numbs(2, N).

filter([], _) -> [];
filter([Head|Tail], N) ->
  if
    Head =< N -> [Head|filter(Tail,N)];
    true -> filter(Tail, N)
  end;
filter(Item, N) when Item =< N ->
  [Item];
filter(_, _) -> [].


reverse([], Acc) -> Acc;
reverse([Head|Tail], Acc) -> reverse(Tail, [Head|Acc]);
reverse(Item, Acc) -> [Item|Acc].

reverse(List) ->
  reverse(List, []).

append([], List) -> List;
append([Head|Tail], List) -> [Head|append(Tail, List)];
append(Item, List) -> [Item|List].

concatenate([]) -> [];
concatenate([Head|Tail]) -> append(Head, concatenate(Tail));
concatenate(Item) -> [Item].

flatten([]) -> [];
flatten([Head|Tail]) -> append(flatten(Head), flatten(Tail));
flatten(Item) -> [Item].
