-module(actors_db).
-export([new/0,
         destroy/1,
         write/3,
         delete/2,
         read/2,
         match/2]).

proc(State) ->
  NewState = receive
    destroy ->
      db:destroy(State),
      exit(self(), destroy);
    {write, {Key, Element}, Pid} ->
      Result = db:write(Key, Element, State),
      Pid ! ok,
      Result;
    {delete, Key, Pid} ->
      Result = db:delete(Key, State),
      Pid ! ok,
      Result;
    {read, Key, Pid} ->
      Pid ! db:read(Key, State),
      State;
    {match, Element, Pid} ->
      Pid ! db:match(Element, State),
      State
  end,
  proc(NewState).

new() ->
  spawn(fun() ->
    proc(db:new())
  end).

destroy(Db) ->
  Db ! destroy,
  ok.

send_and_receive(Msg, Db) ->
  case is_process_alive(Db) of
    false -> {error, db_is_destroy};
    true ->
      Db ! Msg,
      receive Answer -> Answer end
  end.

write(Key, Element, Db) ->
  send_and_receive({write, {Key, Element}, self()}, Db).

delete(Key, Db) ->
  send_and_receive({delete, Key, self()}, Db).

read(Key, Db) ->
  send_and_receive({read, Key, self()}, Db).

match(Element, Db) ->
  send_and_receive({match, Element, self()}, Db).
