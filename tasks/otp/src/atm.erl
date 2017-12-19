-module(atm).
-behaviour(gen_statem).

%%---------------------------------------------------
%%
%% This module implements logic of automated teller machine
%%
%%----------------------------------------------------

%%% exported API
-export([start_link/1,
         insert_card/2,
         push_button/2,
         terminate_atm/2]).

%%% exported internals of gen_statem
-export([init/1,
         terminate/3,
         callback_mode/0,
         waiting_card/3,
         waiting_pin/3,
         select_mode/3,
         withdraw/3,
         deposit/3]).

%%% record
-record(data, {
  current_card,
  input = [],
  attempts = 1
}).

%%% API
-spec start_link(Name :: atom()) ->
  {ok, pid()} | {error, any()}.
start_link(Name) ->
  gen_statem:start_link({local, Name}, ?MODULE, [], []).

-spec insert_card(Name :: atom(),
                  CardNo :: integer()) ->
  ok  | {error, Reason :: any()}.
insert_card(Name, CardNo) ->
  gen_statem:call(Name, {insert_card, CardNo}).

-type user_command() :: enter|withdraw|deposit|cancel.
-type terminal_digit() :: 0|1|2|3|4|5|6|7|8|9.

-spec push_button(Name :: atom(),
                  Button :: user_command() | terminal_digit()) ->
  continue | {ok, Result :: any()} | {error, Reason :: any()}.
push_button(Name, Button) ->
  gen_statem:call(Name, {push_button, Button}).

-spec terminate_atm(Name :: atom(), Reason :: any()) -> ok.
terminate_atm(Name, _Reason) ->
  gen_statem:stop(Name).

%%% internal callbacks
init([]) ->
  {ok, waiting_card, #data{}}.

terminate(_Reason, _State, _Data) ->
  ok.

callback_mode() ->
  state_functions.

waiting_card({call, From}, {insert_card, CardNo}, Data) ->
  handle_validate_card(
    bank:validate_card(CardNo),
    From,
    CardNo,
    Data
  );
waiting_card({call, From}, _StateContent, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}}
  ]}.

waiting_pin({call, From}, {push_button, enter},
            #data{current_card = CardNo, input = Pin} = Data) ->
  {ok, MaxAttempts} = application:get_env(max_attempts),
  handle_validate_pin(
    bank:validate_pin(CardNo, Pin),
    From,
    MaxAttempts,
    Data
  );
waiting_pin({call, From}, {push_button, cancel}, Data) ->
  NewData = reset_data(Data),
  {next_state, waiting_card, NewData, [
    {reply, From, {ok, card_is_return}}
  ]};
waiting_pin(StateEvent, {push_button, Number}, Data) ->
  push_number(StateEvent, Number, waiting_pin, Data);
waiting_pin(state_timeout, _Reason, Data) ->
  NewData = reset_data(Data),
  io:fwrite("time is out~n"),
  {next_state, waiting_card, NewData};
waiting_pin({call, From}, _StateContent, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}},
    get_state_timeout_tuple()
  ]}.

select_mode({call, From}, {push_button, withdraw}, Data) ->
  {next_state, withdraw, Data, [
    {reply, From, {ok, withdraw}}
  ]};
select_mode({call, From}, {push_button, deposit}, Data) ->
  {next_state, deposit, Data, [
    {reply, From, {ok, deposit}}
  ]};
select_mode({call, From}, {push_button, cancel}, Data) ->
  NewData = reset_data(Data),
  {next_state, waiting_card, NewData, [
    {reply, From, {ok, card_is_return}}
  ]};
select_mode({call, From}, _StateContent, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}}
  ]}.

withdraw({call, From}, {push_button, enter},
         #data{current_card = CardNo, input = Input} = Data) ->
  Amount = input_to_integer(Input),
  NewData = reset_input(Data),
  handle_withdraw(
    bank:withdraw(CardNo, Amount),
    From,
    Amount,
    NewData
  );
withdraw({call, From}, {push_button, cancel}, Data) ->
  NewData = reset_input(Data),
  {next_state, select_mode, NewData, [
    {reply, From, {ok, select_mode}}
  ]};
withdraw(StateEvent, {push_button, Number}, Data) ->
  push_number(StateEvent, Number, withdraw, Data);
withdraw({call, From}, _StateContent, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}}
  ]}.

deposit({call, From}, {push_button, enter},
        #data{current_card = CardNo, input = Input} = Data) ->
  Amount = input_to_integer(Input),
  NewData = reset_input(Data),
  Msg = bank:deposit(CardNo, Amount),
  {next_state, select_mode, NewData, [
    {reply, From, Msg}
  ]};
deposit({call, From}, {push_button, cancel}, Data) ->
  NewData = reset_input(Data),
  {next_state, select_mode, NewData, [
    {reply, From, {ok, select_mode}}
  ]};
deposit(StateEvent, {push_button, Number}, Data) ->
  push_number(StateEvent, Number, deposit, Data);
deposit({call, From}, _StateContent, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}}
  ]}.

%%% helpers
handle_validate_card({ok, valid_card}, From, CardNo, Data) ->
  NewData = Data#data{current_card = CardNo},
  {next_state, waiting_pin, NewData, [
    {reply, From, ok},
    get_state_timeout_tuple()
  ]};
handle_validate_card({error, Reason}, From, _CardNo, Data) ->
  {keep_state, Data, [
    {reply, From, {error, Reason}}
  ]}.

handle_validate_pin({ok, valid_pin}, From, _MaxAttempts,
                    #data{input = Pin} = Data) ->
  NewData = reset_input(Data),
  {next_state, select_mode, NewData, [
    {reply, From, {ok, {valid_pin, Pin}}}
  ]};
handle_validate_pin({error, Reason}, From, MaxAttempts,
                    #data{attempts = Attempts} = Data) when Attempts < MaxAttempts ->
  NewData = Data#data{input = [], attempts = Attempts + 1},
  {keep_state, NewData, [
    {reply, From, {error, Reason}},
    get_state_timeout_tuple()
  ]};
handle_validate_pin({error, _Reason}, From, _MaxAttempts, Data) ->
  NewData = reset_data(Data),
  {next_state, waiting_card, NewData, [
    {reply, From, {error, attempts_are_wasted}}
  ]}.

handle_withdraw({ok, {withdraw, Amount}} = Reply, From, Amount, Data) ->
  {next_state, select_mode, Data, [
    {reply, From, Reply}
  ]};
handle_withdraw({error, Reason}, From, _Amount, Data) ->
  {keep_state, Data, [
    {reply, From, {error, Reason}}
  ]}.

push_number({call, From}, Number, waiting_pin,
            #data{input = Input} = Data) when Number >= 0 andalso Number =< 9 ->
  NewData = Data#data{input = Input ++ [Number]},
  {keep_state, NewData, [
    {reply, From, continue},
    get_state_timeout_tuple()
  ]};
push_number({call, From}, _Number, waiting_pin, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}},
    get_state_timeout_tuple()
  ]};
push_number({call, From}, Number, _State,
            #data{input = Input} = Data) when Number >= 0 andalso Number =< 9 ->
  NewData = Data#data{input = Input ++ [Number]},
  {keep_state, NewData, [{reply, From, continue}]};
push_number({call, From}, _Number, _State, Data) ->
  {keep_state, Data, [
    {reply, From, {error, invalid_action}}
  ]}.

get_state_timeout_tuple() ->
  {ok, Timeout} = application:get_env(state_timeout),
  {state_timeout, Timeout, time_is_out}.

reset_data(_) ->
  #data{}.

reset_input(Data) ->
  Data#data{input = []}.

input_to_integer([]) ->
  0;
input_to_integer(Input) ->
  Str = lists:foldl(fun(Item, Acc) ->
    Acc ++ integer_to_list(Item)
  end, "", Input),
  list_to_integer(Str).
