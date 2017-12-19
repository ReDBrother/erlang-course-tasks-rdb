-module(actors_sup).

-export([start_link/0]).

-export([init/1]).

-export([test/0]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {#{strategy => one_for_one,
          intensity => 3,
          period => 60}, []}}.

test() ->
  Pid = election_server:create(100),
  election_server:start_election(Pid),
  register(server, Pid).


