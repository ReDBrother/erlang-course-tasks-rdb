-module(voter).

-export([create/1]).

-export([init/1,
         terminate/1,
         handle_message/2,
         handle_down/3]).

-type voter_status() :: created|candidate|leader|waiting_leader.

-record(state, {
  status = created :: voter_status(),
  election_proc,
  voters = [],
  leader = none
}).

%%---------------------------------------------
%%
%% API
%%
%%---------------------------------------------

create(Voters) ->
  gen_proc:start(?MODULE, [Voters]).

%%---------------------------------------------
%%
%% Callbacks
%%
%%---------------------------------------------

init([Voters]) ->
  [gen_proc:cast(Voter, {add_voter, self()}) || Voter <- Voters],
  [monitor(process, Voter) || Voter <- Voters],
  #state{election_proc = election:create(), voters = Voters}.

terminate(_) ->
  ok.

handle_message({add_voter, Voter}, #state{voters = Voters} = State) ->
  monitor(process, Voter),
  {noreply, State#state{voters = [Voter|Voters]}};
handle_message(start_election, #state{election_proc = ElectionProc,
                                      voters = Voters}) ->
  NewState = start_election(ElectionProc, Voters),
  {reply, ok, NewState};
handle_message(get_election_proc, #state{election_proc = ElectionProc} = State) ->
  {reply, ElectionProc, State};
handle_message({change_status, leader}, #state{voters = Voters} = State) ->
  [gen_proc:cast(Voter, {new_leader, self()}) || Voter <- Voters],
  {noreply, State#state{status = leader, leader = self()}};
handle_message({change_status, not_leader}, State) ->
  {noreply, State#state{status = waiting_leader}};
handle_message({new_leader, Leader}, #state{status = leader} = State) ->
  gen_proc:cast(Leader, {new_round, self()}),
  NewState = start_election(election:create(), [Leader], State),
  {noreply, NewState};
handle_message({new_leader, Leader}, State) ->
  {noreply, State#state{status = leader_selected,
                        election_proc = election:create(),
                        leader = Leader}};
handle_message({new_round, _Voter}, #state{status = candidate} = State) ->
  {noreply, State};
handle_message({new_round, Voter}, State) ->
  NewState = start_election(election:create(), [Voter], State),
  {noreply, NewState};
handle_message(get_status, #state{status = Status} = State) ->
  {reply, Status, State};
handle_message(ping, State) ->
  {reply, {self(), State}, State};
handle_message(terminate, State) ->
  {reply, {terminate, State}, State}.

handle_down(Mref, Leader, #state{election_proc = ElectionProc,
                                 voters = Voters,
                                 leader = Leader}) ->
  demonitor(Mref, [flush]),
  NewState = start_election(ElectionProc, lists:delete(Leader, Voters)),
  {noreply, NewState};
handle_down(Mref, Voter, #state{voters = Voters} = State) ->
  demonitor(Mref, [flush]),
  {noreply, State#state{voters = lists:delete(Voter, Voters)}}.

%%------------------------------------------------
%%
%% Helpers
%%
%%------------------------------------------------

start_election(ElectionProc, Voters) ->
  election:start_election(ElectionProc, Voters),
  #state{status = candidate,
         election_proc = ElectionProc,
         voters = Voters,
         leader = none}.

start_election(ElectionProc, Candidates, State) ->
  election:start_election(ElectionProc, Candidates),
  State#state{status = candidate,
              election_proc = ElectionProc,
              leader = none}.
