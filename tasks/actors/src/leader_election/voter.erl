-module(voter).

-export([create/3, init/1, terminate/1, handle_message/2, handle_down/3]).

-record(state, {
  status = created,
  config = {},
  voters = [],
  leader = none
}).

create(Config, Voters, Server) ->
  gen_proc:start(?MODULE, [Config, Voters, Server]).

start_election(State) ->
  Self = self(),
  spawn(fun() ->
    Voters = State#state.voters,
    {Value, _} = State#state.config,
    Ref = make_ref(),
    [Voter ! {Ref, self(), {im_leader, Value}} || Voter <- Voters],
    receive
      {Ref, no} ->
        Self ! {Ref, self(), {change_status, candidate}}
    after 500 ->
      Self ! {Ref, self(), {change_status, leader}}
    end
  end).

send_all(Voters, Request) ->
  [{gen_proc:send(Voter, Request), monitor(process, Voter)} || Voter <- Voters].

init([Config, Voters, Server]) ->
  send_all(Voters, {add_voter, self()}),
  Server ! {voter_created, self()},
  #state{config = Config, voters = Voters}.

terminate(Voter) ->
  gen_proc:send(Voter, terminate).

handle_message(start_election, State) ->
  Status = State#state.status,
  case Status of
    created ->
      start_election(State);
    _ ->
      ok
  end,
  {reply, ok, State};
handle_message({add_voter, Voter}, State) ->
  Voters = State#state.voters,
  {reply, ok, State#state{voters = [Voter|Voters]}};
handle_message({change_status, leader}, State) ->
  Voters = State#state.voters,
  send_all(Voters, {new_leader, self()}),
  {noreply, State#state{status = leader, leader = self()}};
handle_message({change_status, Status}, State) ->
  {noreply, State#state{status = Status}};
handle_message({new_leader, Leader}, State) ->
  {reply, ok, State#state{status = leader_selected, leader = Leader}};
handle_message({im_leader, Value}, State) ->
  {MyValue, Pred} = State#state.config,
  case Pred(MyValue, Value) of
    true ->
      {reply, yes, State};
    false ->
      {reply, no, State}
  end;
handle_message(is_leader, State) ->
  Status = State#state.status,
  case Status of
    leader ->
      {reply, true, State};
    _ ->
      {reply, false, State}
  end;
handle_message(ping, State) ->
  {reply, {self(), State}, State};
handle_message(terminate, State) ->
  {reply, {terminate, State}, State}.

handle_down(Mref, Voter, State) ->
  demonitor(Mref, [flush]),
  Voters = State#state.voters,
  Leader = State#state.leader,
  NewVoters = lists:delete(Voter, Voters),
  NewState = State#state{voters = NewVoters},
  case Voter == Leader of
    true ->
      start_election(NewState),
      {noreply, NewState#state{status = leader_not_selected, leader = none}};
    false ->
      {noreply, NewState}
  end.
