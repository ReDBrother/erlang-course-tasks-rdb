-module(actors_db).
-export([new/0,
         destroy/1,
         write/3,
         delete/2,
         read/2,
         match/2]).

proc(State) ->
  NewState = receive
    {Operation, Opts, Pid} ->
      case get_result(Operation, Opts, State) of
        exit ->
          exit(self(), Operation);
        {ok, Result} ->
          Pid ! {self(), Result},
          State;
        {error, Reason} ->
          Pid ! {self(), {error, Reason}},
          State;
        NewDbState ->
          Pid ! {self(), ok},
          NewDbState
      end
  end,
  proc(NewState).

get_result(destroy, _, State) ->
  db:destroy(State),
  exit;
get_result(FuncName, {Key, Element}, State) ->
  db:FuncName(Key, Element, State);
get_result(FuncName, Arg, State) ->
  db:FuncName(Arg, State).

new() ->
  spawn(fun() ->
    proc(db:new())
  end).

destroy(Db) ->
  Db ! {destroy, {}, self()},
  ok.

send_and_receive(Msg, Db) ->
  case is_process_alive(Db) of
    false -> {error, db_is_destroy};
    true ->
      Db ! Msg,
      receive {Db, Answer} -> Answer end
  end.

write(Key, Element, Db) ->
  send_and_receive({write, {Key, Element}, self()}, Db).

delete(Key, Db) ->
  send_and_receive({delete, Key, self()}, Db).

read(Key, Db) ->
  send_and_receive({read, Key, self()}, Db).

match(Element, Db) ->
  send_and_receive({match, Element, self()}, Db).
