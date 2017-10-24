-module(election_server).

-export([create_server/1,
         start_election/1,
         get_voters/1,
         get_leader/1,
         is_leader/2,
         init/1,
         terminate/1,
         handle_message/2,
         handle_down/3]).

create_server(NumVoters) when NumVoters > 0 ->
  Pred = fun(X, Y) -> X < Y end,
  gen_proc:start(?MODULE, [NumVoters, Pred]).

create_voters(0, _, Voters) ->
  Voters;
create_voters(N, Pred, Voters) ->
  Voter = create({N, Pred}, Voters),
  create_voters(N - 1, Pred, [Voter|Voters]).

create(Config, Voters) ->
  Voter = voter:create(Config, Voters, self()),
  monitor(process, Voter),
  receive {voter_created, Voter} -> Voter end.

start_election(Server) ->
  gen_proc:send(Server, start_election).

get_voters(Server) ->
  gen_proc:send(Server, get_voters).

get_leader(Server) ->
  gen_proc:send(Server, get_leader).

is_leader(Server, Voter) ->
  gen_proc:send(Server, {is_leader, Voter}).

init([N, Pred]) ->
  create_voters(N, Pred, []).

terminate(_) ->
  ok.

handle_message(start_election, Voters) ->
  [gen_proc:send(Voter, start_election) || Voter <- Voters],
  {reply, ok, Voters};
handle_message(get_voters, Voters) ->
  Result = lists:map(fun(V) ->
    gen_proc:send(V, ping) end, Voters),
  {reply, Result, Voters};
handle_message(get_leader, Voters) ->
  Result = [Voter || Voter <- Voters, gen_proc:send(Voter, is_leader)],
  case Result of
    [] ->
      {reply, {ok, not_selected}, Voters};
    [Voter] ->
      {reply, gen_proc:send(Voter, ping), Voters};
    _ ->
      {reply, {error, {unexpected, Result}}, Voters}
  end;
handle_message({is_leader, Voter}, Voters) ->
  case lists:any(fun(V) -> V == Voter end, Voters) of
    true ->
      Result = gen_proc:send(Voter, is_leader),
      {reply, {ok, Result}, Voters};
    false ->
      {reply, {error, not_found_voter}, Voters}
  end.

handle_down(Mref, Voter, Voters) ->
  demonitor(Mref, [flush]),
  NewVoters = lists:delete(Voter, Voters),
  {noreply, NewVoters}.
