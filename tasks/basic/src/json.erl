-module(json).
-export([new/1, read/2, write/3]).

new([]) -> #{};
new([{Key, ValueSpec}|Tail]) ->
  Obj = #{Key => createValue(ValueSpec, #{})},
  createValue(Tail, Obj).

createValue([], #{}) -> [];
createValue([], Obj) -> Obj;
createValue([{Key, ValueSpec}|Tail], Obj) ->
  NewObj = maps:put(Key, createValue(ValueSpec, #{}), Obj),
  createValue(Tail, NewObj);
createValue({Key, ValueSpec}, _) ->
  #{Key => createValue(ValueSpec, #{})};
createValue([_|_] = Arr, _) -> createArray(Arr, []);
createValue(BasicValue, _) -> BasicValue.

createArray([], Acc) -> Acc;
createArray([Head|Tail], Acc) ->
  createArray(Tail, Acc ++ [createValue(Head, #{})]).

read(KeySpec, JsonObj) ->
  case maps:is_key(KeySpec, JsonObj) of
    true ->
      Result = maps:get(KeySpec, JsonObj),
      {ok, Result};
    false ->
      {error, not_found}
  end.

write(KeySpec, ValueSpec, JsonObj) ->
  case maps:is_key(KeySpec, JsonObj) of
    true ->
      maps:update(KeySpec, createValue(ValueSpec, #{}), JsonObj);
    false ->
      {error, not_found}
  end.
