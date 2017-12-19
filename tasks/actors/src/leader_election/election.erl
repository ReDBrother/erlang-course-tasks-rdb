-module(election).

-export([create/0, start_election/2]).

%%-------------------------------------------
%%
%% API
%%
%%-------------------------------------------

create() ->
  Voter = self(),
  spawn(fun() ->
    receive
      {Voter, {start, Candidates}} ->
        start(Voter, Candidates)
    end
  end).

start_election(ElectionProc, Candidates) ->
  ElectionProc ! {self(), {start, Candidates}}.

%%--------------------------------------------
%%
%% Helpers
%%
%%--------------------------------------------

start(Voter, Candidates) ->
  ElectionProcs = [gen_proc:call(Candidate, get_election_proc)
                  || Candidate <- Candidates, is_process_alive(Candidate)],
  ElectionId = 1,
  start(Voter, ElectionProcs, ElectionId).

start(Voter, ElectionProcs, ElectionId) ->
  case start_election_round(ElectionProcs, ElectionId) of
    im_leader ->
      gen_proc:cast(Voter, {change_status, leader});
    im_not_leader ->
      gen_proc:cast(Voter, {change_status, not_leader});
    {start_next_round, Rest} ->
      start(Voter, Rest, ElectionId + 1)
  end.

start_election_round([], _ElectionId) ->
  im_leader;
start_election_round(ElectionProcs, ElectionId) ->
  Options = {ElectionId, self(), rand:uniform(2)},
  [ElectionProc ! Options || ElectionProc <- ElectionProcs],
  receive_votes([], Options).

receive_votes(Rest, {ElectionId, _Self, Number} = Options) ->
  receive
    {ElectionId, ElectionProc, Number} ->
      receive_votes([ElectionProc|Rest], Options);
    {ElectionId, _ElectionProc, CandidateNumber}
      when CandidateNumber < Number ->
      receive_votes(Rest, Options);
    {ElectionId, _ElectionProc, CandidateNumber}
      when CandidateNumber > Number ->
      im_not_leader
  after 100 ->
    {start_next_round, Rest}
  end.
