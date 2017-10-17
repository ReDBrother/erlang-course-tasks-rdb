-module(db).
-export([new/0,
         new/1,
         destroy/1,
         write/3,
         delete/2,
         read/2,
         match/2,
         append/3,
         batch_delete/2,
         batch_read/2]).

create_config([], Opts) -> Opts;
create_config([{append, Flag}|Tail], Opts) ->
  NewOpts = maps:put(append, Flag, Opts),
  create_config(Tail, NewOpts);
create_config([{batch, Number}|Tail], Opts) ->
  NewOpts = case Number > 0 of
    true ->
      maps:put(batch, Number, Opts);
    false ->
      maps:put(batch, 1, Opts)
  end,
  create_config(Tail, NewOpts).

new() ->
  #{config => #{}, state => []}.

new(Parameters) ->
  #{config => create_config(Parameters, #{}), state => []}.

destroy(_) ->
  ok.

write(Key, Element, Db) ->
  State = maps:get(state, Db),
  NewState = write_item(Key, Element, State),
  maps:update(state, NewState, Db).

write_item(Key, Element, State) ->
  [{Key, Element}|State].

delete(Key, Db) ->
  State = maps:get(state, Db),
  NewState = delete_item(Key, State),
  maps:update(state, NewState, Db).

delete_item(_, []) -> [];
delete_item(Key, [Head|Tail]) ->
  case Head of
    {Key, _} -> Tail;
    _ -> [Head|delete_item(Key, Tail)]
  end.

read(Key, Db) ->
  State = maps:get(state, Db),
  find_element(Key, State).

find_element(_, []) -> {error, instance};
find_element(Key, [Head|Tail]) ->
  case Head of
    {Key, Element} -> {ok, Element};
    _ -> find_element(Key, Tail)
  end.

match(Element, Db) ->
  State = maps:get(state, Db),
  match_element(Element, State).

match_element(_, []) -> [];
match_element(Element, [Head|Tail]) ->
  case Head of
    {Key, Element} -> [Key|match_element(Element, Tail)];
    _ -> match_element(Element, Tail)
  end.

append(Key, Element, Db) ->
  State = maps:get(state, Db),
  Config = maps:get(config, Db),
  Result = case maps:is_key(append, Config) of
    false ->
      append(Key, Element, deny, State);
    true ->
      append(Key, Element, maps:get(append, Config), State)
  end,
  case Result of
    {ok, NewState} ->
      maps:update(state, NewState, Db);
    {error, Reason} -> {error, Reason}
  end.

append(_, _, deny, _) -> {error, append_deny};
append(Key, Element, allow, State) -> {ok, State ++ [{Key, Element}]}.

batch_delete(KeyList, Db) ->
  State = maps:get(state, Db),
  Config = maps:get(config, Db),
  Result = case maps:is_key(batch, Config) of
    false ->
      batch_delete(KeyList, 1, State);
    true ->
      batch_delete(KeyList, maps:get(batch, Config), State)
  end,
  case Result of
    {ok, NewState} ->
      maps:update(state, NewState, Db);
    {error, Reason} -> {error, Reason}
  end.

batch_delete(KeyList, Limit, _) when length(KeyList) > Limit ->
  {error, batch_limit};
batch_delete([], _, State) -> {ok, State};
batch_delete([Key|Tail], Limit, State) ->
  NewState = delete_item(Key, State),
  batch_delete(Tail, Limit, NewState).

batch_read(KeyList, Db) ->
  State = maps:get(state, Db),
  Config = maps:get(config, Db),
  Result = case maps:is_key(batch, Config) of
    false ->
      batch_read(KeyList, 1, State, []);
    true ->
      batch_read(KeyList, maps:get(batch, Config), State, [])
  end,
  case Result of
    {ok, Items} -> Items;
    {error, Reason} -> {error, Reason}
  end.

batch_read(KeyList, Limit, _, _) when length(KeyList) > Limit ->
  {error, batch_limit};
batch_read([], _, _, Acc) -> {ok, lists:reverse(Acc)};
batch_read([Key|Tail], Limit, State, Acc) ->
  case find_element(Key, State) of
    {ok, Element} ->
      batch_read(Tail, Limit, State, [{Key, Element}|Acc]);
    {error, instance} -> {error, instance}
  end.
