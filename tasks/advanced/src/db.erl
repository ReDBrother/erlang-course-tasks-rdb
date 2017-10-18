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

create_config([], Opts) ->
  Opts;
create_config([{append, Flag}|Tail], Opts) ->
  create_config(Tail, Opts#{append => Flag});
create_config([{batch, Number}|Tail], Opts) ->
  N = case Number > 0 of
    true ->
      Number;
    false ->
      1
  end,
  create_config(Tail, Opts#{batch => N}).

get_config_value(Key, Opts, Default) ->
  case maps:is_key(Key, Opts) of
    true -> maps:get(Key, Opts);
    false -> Default
  end.

get_append_flag(Opts) ->
  get_config_value(append, Opts, deny).

get_batch_limit(Opts) ->
  get_config_value(batch, Opts, 1).

new() ->
  #{config => #{},
    state => []}.

new(Parameters) ->
  #{config => create_config(Parameters, #{}),
    state => []}.

destroy(_) ->
  ok.

write(Key, Element, Db) ->
  State = maps:get(state, Db),
  NewState = write_item({Key, Element}, State),
  Db#{state => NewState}.

write_item(Item, State) ->
  [Item|State].

delete(Key, Db) ->
  State = maps:get(state, Db),
  NewState = delete_item(Key, State),
  Db#{state => NewState}.

delete_item(_, []) ->
  [];
delete_item(Key, [Head|Tail]) ->
  case Head of
    {Key, _} -> Tail;
    _ -> [Head|delete_item(Key, Tail)]
  end.

read(Key, Db) ->
  State = maps:get(state, Db),
  get_element(Key, State).

get_element(_, []) ->
  {error, instance};
get_element(Key, [Head|Tail]) ->
  case Head of
    {Key, Element} ->
      {ok, Element};
    _ ->
      get_element(Key, Tail)
  end.

match(Element, Db) ->
  State = maps:get(state, Db),
  {ok, match_element(Element, State)}.

match_element(_, []) ->
  [];
match_element(Element, [Head|Tail]) ->
  case Head of
    {Key, Element} ->
      [Key|match_element(Element, Tail)];
    _ ->
      match_element(Element, Tail)
  end.

append(Key, Element, Db) ->
  State = maps:get(state, Db),
  Config = maps:get(config, Db),
  Flag = get_append_flag(Config),
  Result = append_item({Key, Element}, Flag, State),
  case Result of
    {ok, NewState} ->
      Db#{state => NewState};
    {error, Reason} ->
      {error, Reason}
  end.

append_item(_, deny, _) ->
  {error, append_deny};
append_item(Item, allow, State) ->
  {ok, State ++ [Item]}.

batch_delete(KeyList, Db) ->
  State = maps:get(state, Db),
  Config = maps:get(config, Db),
  Limit = get_batch_limit(Config),
  Result = batch_delete(KeyList, length(KeyList) =< Limit, State),
  case Result of
    {ok, NewState} ->
      Db#{state => NewState};
    {error, Reason} ->
      {error, Reason}
  end.

batch_delete(_, false, _) ->
  {error, batch_limit};
batch_delete([], _, State) ->
  {ok, State};
batch_delete([Key|Tail], _, State) ->
  NewState = delete_item(Key, State),
  batch_delete(Tail, true, NewState).

batch_read(KeyList, Db) ->
  State = maps:get(state, Db),
  Config = maps:get(config, Db),
  Limit = get_batch_limit(Config),
  Result = batch_read(KeyList, length(KeyList) =< Limit, State, []),
  case Result of
    {ok, Items} ->
      Items;
    {error, Reason} ->
      {error, Reason}
  end.

batch_read(_, false, _, _) ->
  {error, batch_limit};
batch_read([], _, _, Acc) ->
  {ok, lists:reverse(Acc)};
batch_read([Key|Tail], _, State, Acc) ->
  case get_element(Key, State) of
    {ok, Element} ->
      batch_read(Tail, true, State, [{Key, Element}|Acc]);
    {error, instance} ->
      {error, instance}
  end.
