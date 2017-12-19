-module(otp_sup).
-behaviour(supervisor).

%%% exported API
-export([start_link/1]).

%%%internals of supervisor
-export([init/1]).


%% API functions

start_link(Cards) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Cards]).

%% Internal functions

init([Cards]) ->
  SupFlags = #{strategy => rest_for_one,
               intensity => 3,
               period => 60},
  ChildSpecs = [#{id => bank,
                 start => {bank, start_link, [Cards]},
                 restart => permanent,
                 shutdown => brutal_kill,
                 type => worker,
                 modules => [bank]},
               #{id => atm_sup,
                 start => {atm_sup, start_link, []},
                 restart => permanent,
                 shutdown => brutal_kill,
                 type => supervisor,
                 modules => [atm_sup]}],
  {ok, {SupFlags, ChildSpecs}}.
