-module(atm_sup).
-behaviour(supervisor).

%%% exported API
-export([start_link/0,
         start_atm/1]).

%%% exported internals of supervisor
-export([init/1]).

%% API functions
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_atm(Name) ->
  supervisor:start_child(?MODULE, [Name]).

%% Internal functions
init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 3,
               period => 60},
  ChildSpecs = [#{id => atm,
                  start => {atm, start_link, []},
                  restart => permanent,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [atm]}],
  {ok, {SupFlags, ChildSpecs}}.
