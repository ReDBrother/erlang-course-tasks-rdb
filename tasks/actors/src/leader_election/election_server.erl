-module(election_server).

-export([create/1,
         start_election/1,
         get_voters/1,
         get_leader/1]).

-export([init/1,
         terminate/1,
         handle_message/2,
         handle_down/3]).

%%------------------------------------------
%%
%% API
%%
%%------------------------------------------

create(NumVoters) when NumVoters > 0 ->
  gen_proc:start(?MODULE, [NumVoters]).

start_election(Server) ->
  gen_proc:call(Server, start_election).

get_voters(Server) ->
  gen_proc:call(Server, get_voters).

get_leader(Server) ->
  gen_proc:call(Server, get_leader).

%%-------------------------------------------
%%
%% Callbacks
%%
%%-------------------------------------------

init([N]) ->
  create_voters(N, []).

terminate(_) ->
  ok.

handle_message(start_election, Voters) ->
  [gen_proc:call(Voter, start_election) || Voter <- Voters],
  {reply, ok, Voters};
handle_message(get_voters, Voters) ->
  Result = [gen_proc:call(Voter, ping) || Voter <- Voters],
  {reply, Result, Voters};
handle_message(get_leader, Voters) ->
  Result = [Voter || Voter <- Voters, gen_proc:call(Voter, get_status) == leader],
  case Result of
    [] ->
      {reply, {ok, not_selected}, Voters};
    [Voter] ->
      {reply, gen_proc:call(Voter, ping), Voters};
    _ ->
      {reply, {error, {unexpected, Result}}, Voters}
  end.

handle_down(Mref, Voter, Voters) ->
  demonitor(Mref, [flush]),
  NewVoters = lists:delete(Voter, Voters),
  {noreply, NewVoters}.

%%--------------------------------------------
%%
%% Helpers
%%
%%--------------------------------------------

create_voters(0, Voters) ->
  Voters;
create_voters(N, Voters) ->
  Voter = voter:create(Voters),
  monitor(process, Voter),
  create_voters(N - 1, [Voter|Voters]).

