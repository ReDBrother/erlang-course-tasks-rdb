-module(gen_db).
-export([new/1,
         delete/1,
         delete/2,
         delete_all_objects/1,
         insert/3,
         find/2]).

-export([init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

-spec new(Name :: atom()) -> ok | {error, Reason :: term()}.
new(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []),
  ok.

-spec delete(Name :: atom()) -> ok.
delete(Name) ->
  gen_server:cast(Name, delete).

-spec delete(Name :: atom(), Key :: term()) -> ok.
delete(Name, Key) ->
  gen_server:cast(Name, {delete, Key}).

-spec delete_all_objects(Name :: atom()) -> ok.
delete_all_objects(Name) ->
  gen_server:cast(Name, delete_all_objects).

-spec insert(Name :: atom(), Key :: term(), Value :: term()) -> ok.
insert(Name, Key, Value) ->
  gen_server:cast(Name, {insert, Key, Value}).

-spec find(Name :: atom(), Key :: term()) -> {ok, Value :: term()} | not_found.
find(Name, Key) ->
  gen_server:call(Name, {find, Key}).

init(_) ->
  {ok, #{}}.

handle_cast(delete, _State) ->
  {stop, delete, #{}};
handle_cast({delete, Key}, State) ->
  NewState = maps:remove(Key, State),
  {noreply, NewState};
handle_cast(delete_all_objects, _State) ->
  {noreply, #{}};
handle_cast({insert, Key, Value}, State) ->
  {noreply, maps:put(Key, Value, State)}.

handle_call({find, Key}, _From, State) ->
  case maps:find(Key, State) of
    error ->
      {reply, not_found, State};
    Result ->
      {reply, Result, State}
  end.
