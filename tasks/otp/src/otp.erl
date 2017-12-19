-module(otp).
-behaviour(application).

%% API exports
-export([start/2, stop/1]).

%% API functions
start(_State, _Args) ->
  Cards = [{1, [1,2,3,4], 100},
           {2, [2,1], 1000},
           {3, [4,3,2,1], 500}],
  otp_sup:start_link(Cards).

stop(_State) ->
  ok.

%% Internal functions
