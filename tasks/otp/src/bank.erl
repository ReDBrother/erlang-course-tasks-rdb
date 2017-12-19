-module(bank).
-behaviour(gen_server).

%%---------------------------------------------------
%%
%% This module implements logic of cards information storage
%%
%%----------------------------------------------------

%%% exported API
-export([start_link/1,
         validate_card/1,
         validate_pin/2,
         withdraw/2,
         deposit/2,
         terminate_bank/1]).

%%% exported internals of gen_server
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

%%% API
-spec start_link([{CardNo :: integer(), Pin :: list(integer()), Balance :: integer()}]) ->
  {ok, pid()} | {error, any()}.
start_link(Cards) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Cards], []).

-spec validate_card(CardNo :: integer()) -> {ok, Result :: atom()} | {error, Reason :: any()}.
validate_card(CardNo) ->
  gen_server:call(?MODULE, {validate_card, CardNo}).

-spec validate_pin(CardNo :: integer(), Pin :: list(integer())) -> {ok, Result :: atom()} | {error, Reason :: any()}.
validate_pin(CardNo, Pin) ->
  gen_server:call(?MODULE, {validate_pin, CardNo, Pin}).

-spec withdraw(CardNo :: integer(), Amount :: integer()) -> {ok, Result :: term()} | {error, Reason :: any()}.
withdraw(CardNo, Amount) ->
  gen_server:call(?MODULE, {withdraw, CardNo, Amount}).

-spec deposit(CardNo :: integer(), Amount :: integer()) -> {ok, Result :: term()} | {error, Reason :: any()}.
deposit(CardNo, Amount) ->
  gen_server:call(?MODULE, {deposit, CardNo, Amount}).

terminate_bank(_) ->
  gen_server:stop(?MODULE).

%%% internal callbacks
init([Cards]) ->
  {ok, Cards}.

terminate(_Reason, _State) ->
  ok.

handle_call({validate_card, CardNo}, _From, Cards) ->
  case lists:keyfind(CardNo, 1, Cards) of
    false ->
      {reply, {error, invalid_card}, Cards};
    _Card ->
      {reply, {ok, valid_card}, Cards}
  end;
handle_call({validate_pin, CardNo, Pin}, _From, Cards) ->
  case lists:keyfind(CardNo, 1, Cards) of
    {_CardNo, Pin, _Balance} ->
      {reply, {ok, valid_pin}, Cards};
    _Any ->
      {reply, {error, {invalid_pin, Pin}}, Cards}
  end;
handle_call({withdraw, CardNo, Amount}, _From, Cards) ->
  case lists:keyfind(CardNo, 1, Cards) of
    {CardNo, Pin, Balance} when Balance >= Amount ->
      NewCards = change_card({CardNo, Pin, Balance - Amount}, Cards),
      {reply, {ok, {withdraw, Amount}}, NewCards};
    {_CardNo, _Pin, _Balance} ->
      {reply, {error, {insufficient_funds, Amount}}, Cards};
    _Any ->
      {reply, {error, invalid_action}, Cards}
  end;
handle_call({deposit, CardNo, Amount}, _From, Cards) ->
  case lists:keyfind(CardNo, 1, Cards) of
    {CardNo, Pin, Balance} ->
      NewCards = change_card({CardNo, Pin, Balance + Amount}, Cards),
      {reply, {ok, {deposit, Amount}}, NewCards};
    false ->
      {reply, {error, invalid_action}, Cards}
  end.

handle_cast(_Content, Cards) ->
  {noreply, Cards}.

handle_info(_Content, Cards) ->
  {noreply, Cards}.

%%helpers

change_card({CardNo, _Pin, _} = NewCard, Cards) ->
  lists:keyreplace(CardNo, 1, Cards, NewCard).
