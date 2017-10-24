-module(gen_proc).

-export([start/2, send/2, loop/2]).

start(Module, InitArgs) ->
  spawn(fun() ->
    State = Module:init(InitArgs),
    LoopState = loop(Module, State),
    Module:terminate(LoopState)
  end).

send(Proc, Request) ->
  Ref = make_ref(),
  Proc ! {Ref, self(), Request},
  receive
    {Ref, Message} ->
      Message
  after 5000 ->
    {error, time_is_out}
  end.

loop(Module, State) ->
  receive
    {'DOWN', MRef, process, Pid, _} ->
      try Module:handle_down(MRef, Pid, State) of
        {terminate, NewState} ->
          NewState;
        {noreply, NewState} ->
          loop(Module, NewState)
      catch
        _ ->
          loop(Module, State)
      end;
    {Ref, From, Msg} ->
      try Module:handle_message(Msg, State) of
        {terminate, NewState} ->
          NewState;
        {reply, Reply, NewState} ->
          From ! {Ref, Reply},
          loop(Module, NewState);
        {noreply, NewState} ->
          loop(Module, NewState)
      catch
        ErrClass:Reason ->
          From ! {Ref, {ErrClass, Reason}},
          loop(Module, State)
      end
  end.
