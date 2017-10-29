-module(atm).
-behaviour(gen_statem).

-export([insert_card/1, push_button/1]).

-export([start_link/1,
         init/1,
         terminate/1,
         terminate/3,
         callback_mode/0,
         waiting_card/3,
         waiting_pin/3,
         mode/3,
         withdraw/3,
         deposit/3,
         handle_event/4]).

-record(data, {
  cards = [],
  current_card = {},
  input = "",
  attempts = 0
}).

-spec start_link([{CardNo :: integer(), Pin :: integer(), Balance :: integer()}]) ->
  {ok, pid()} | {error, any()}.
start_link(Cards) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [Cards], []).

-spec insert_card(CardNo :: integer()) -> ok  | {error, Reason :: term()}.
insert_card(CardNo) ->
  gen_statem:call(?MODULE, {insert_card, CardNo}).

-spec push_button(Button :: enter|withdraw|deposit|cancel|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')->
  continue | {ok, Result :: term()} | {error, Reason :: term()}.
push_button(Button) ->
  gen_statem:call(?MODULE, {push_button, Button}).

init([Cards]) ->
  Data = #data{cards = Cards},
  {ok, waiting_card, Data}.

terminate(_) ->
  gen_statem:stop(?MODULE).

terminate(_Reason, _State, _Data) ->
  ok.

callback_mode() -> state_functions.

get_state_timeout_tuple() ->
  {state_timeout, 10000, time_is_out}.

get_default_data(Data) ->
  Cards = Data#data.cards,
  #data{cards = Cards}.

waiting_card({call, From}, {insert_card, CardNo}, Data) ->
  Cards = Data#data.cards,
  case lists:keyfind(CardNo, 1, Cards) of
    false ->
      Reply = {error, invalid_card},
      io:fwrite("Карточка недействительна (CardNo: ~p)~n", [CardNo]),
      {keep_state, Data, [{reply, From, Reply}]};
    Card ->
      NewData = Data#data{current_card = Card},
      Reply = ok,
      io:fwrite("Вы вставили карточку ~p~n", [CardNo]),
      io:fwrite("[~p] Введите ваш пин код ~n", [CardNo]),
      {next_state, waiting_pin, NewData, [
        {reply, From, Reply},
        get_state_timeout_tuple()
      ]}
  end;
waiting_card(StateEvent, _StateContent, Data) ->
  handle_event(StateEvent, {error, invalid_action}, waiting_card, Data).

waiting_pin({call, From}, {insert_card, _CardNo}, Data) ->
  {CurrentCardNo, _, _} = Data#data.current_card,
  Reply = {error, {already_have_card, CurrentCardNo}},
  {keep_state, Data, [
    {reply, From, Reply},
    get_state_timeout_tuple()
  ]};
waiting_pin(StateEvent, {push_button, cancel}, Data) ->
  handle_event(StateEvent, {ok, card_is_return}, waiting_pin, Data);
waiting_pin({call, From}, {push_button, enter}, Data) ->
  {CardNo, Pin, _} = Data#data.current_card,
  Input = Data#data.input,
  case integer_to_list(Pin) =:= Input of
    true ->
      NewData = Data#data{input = "0"},
      Reply = {ok, valid_pin},
      io:fwrite("[~p] ~p - верный пинкод~n", [CardNo, Input]),
      io:fwrite("Выберите операцию [withdraw/deposit]~n"),
      {next_state, mode, NewData, [{reply, From, Reply}]};
    false ->
      io:fwrite("[~p] Вы ввели неверный пинкод ~p~n", [CardNo, Input]),
      handle_event({call, From}, {error, invalid_pin}, waiting_pin, Data)
  end;
waiting_pin(StateEvent, {push_button, Number}, Data) when Number >= '0' andalso Number =< '9' ->
  handle_event(StateEvent, {change_input, Number}, waiting_pin, Data);
waiting_pin(StateEvent, time_is_out, Data) ->
  handle_event(StateEvent, {error, time_is_out}, waiting_pin, Data);
waiting_pin(StateEvent, _StateContent, Data) ->
  handle_event(StateEvent, {error, invalid_action}, waiting_pin, Data).

mode({call, From}, {push_button, withdraw}, Data) ->
  Reply = {ok, withdraw},
  io:fwrite("Введите Сумму, которую хотите снять~n"),
  {next_state, withdraw, Data, [{reply, From, Reply}]};
mode({call, From}, {push_button, deposit}, Data) ->
  Reply = {ok, deposit},
  io:fwrite("Введите Сумму, которую хотите ввести~n"),
  {next_state, deposit, Data, [{reply, From, Reply}]};
mode(StateEvent, {push_button, cancel}, Data) ->
  handle_event(StateEvent, {ok, card_is_return}, mode, Data);
mode({call, From}, _StateContent, Data) ->
  Reply = {error, invalid_action},
  {keep_state, Data, [{reply, From, Reply}]}.

withdraw({call, From}, {push_button, enter}, Data) ->
  Cards = Data#data.cards,
  {CardNo, Pin, Money} = Data#data.current_card,
  Input = Data#data.input,
  Amount = list_to_integer(Input),
  case Money >= Amount of
    true ->
      NewCard = {CardNo, Pin, Money - Amount},
      Cards = Data#data.cards,
      NewCards = lists:keyreplace(CardNo, 1, Cards, NewCard),
      NewData = Data#data{cards = NewCards,
        current_card = NewCard,
        input = "0"
      },
      Reply = {ok, {withdraw, Amount}},
      io:fwrite("[~p] Вы сняли со счета ~p~n", [CardNo, Amount]),
      io:fwrite("[~p] У вас на счету ~p~n", [CardNo, Money - Amount]),
      io:fwrite("Выберите операцию [withdraw/deposit]~n"),
      {next_state, mode, NewData, [{reply, From, Reply}]};
    false ->
      NewData = Data#data{input = "0"},
      Reply = {error, invalid_amount},
      io:fwrite("[~p] Недостаточно средств на счету (Money: ~p, Input: ~p) ~n", [CardNo, Money, Amount]),
      io:fwrite("Введите сумму, которую хотите снять~n"),
      {keep_state, NewData, [{reply, From, Reply}]}
  end;
withdraw(StateEvent, {push_button, cancel}, Data) ->
  handle_event(StateEvent, select_mode, withdraw, Data);
withdraw(StateEvent, {push_button, Number}, Data) when Number >= '0' andalso Number =< '9' ->
  handle_event(StateEvent, {change_input, Number}, withdraw, Data);
withdraw({call, From}, _StateContent, Data) ->
  Reply = {error, invalid_action},
  {keep_state, Data, [{reply, From, Reply}]}.

deposit({call, From}, {push_button, enter}, Data) ->
  Cards = Data#data.cards,
  {CardNo, Pin, Money} = Data#data.current_card,
  Input = Data#data.input,
  Amount = list_to_integer(Input),
  NewCard = {CardNo, Pin, Money + Amount},
  NewCards = lists:keyreplace(CardNo, 1, Cards, NewCard),
  NewData = Data#data{cards = NewCards,
    current_card = NewCard,
    input = "0"
  },
  Reply = {ok, {deposit, Amount}},
  io:fwrite("[~p] Вы положили на счет ~p~n", [CardNo, Amount]),
  io:fwrite("[~p] У вас на счету ~p~n", [CardNo, Money + Amount]),
  io:fwrite("Выберите операцию [withdraw/deposit]~n"),
  {next_state, mode, NewData, [{reply, From, Reply}]};
deposit(StateEvent, {push_button, cancel}, Data) ->
  handle_event(StateEvent, select_mode, deposit, Data);
deposit(StateEvent, {push_button, Number}, Data) ->
  handle_event(StateEvent, {change_input, Number}, deposit, Data);
deposit({call, From}, _StateContent, Data) ->
  Reply = {error, invalid_action},
  {keep_state, Data, [{reply, From, Reply}]}.

handle_event(state_timeout, _StateContent, waiting_pin, Data) ->
  NewData = get_default_data(Data),
  io:fwrite("Время вышло, забирайте карту~n"),
  {next_state, waiting_card, NewData};
handle_event({call, From}, {change_input, Number}, State, Data) ->
  Input = Data#data.input,
  NewData = Data#data{input = Input ++ atom_to_list(Number)},
  io:fwrite("Вы нажали ~p~n", [Number]),
  case State of
    waiting_pin ->
      {keep_state, NewData, [
        {reply, From, continue},
        get_state_timeout_tuple()
      ]};
    _ ->
      {keep_state, NewData, [{reply, From, continue}]}
  end;
handle_event({call, From}, {error, invalid_pin} = StateContent, waiting_pin, Data) ->
  Attempts = Data#data.attempts + 1,
  case Attempts of
    3 ->
      NewData = get_default_data(Data),
      {next_state, waiting_card, NewData, [{reply, From, StateContent}]};
    _ ->
      NewData = Data#data{attempts = Attempts},
      Reply = {error, 3 - Attempts},
      {keep_state, NewData, [
        {reply, From, Reply},
        get_state_timeout_tuple()
      ]}
  end;
handle_event({call, From}, select_mode, _State, Data) ->
  Reply = {ok, mode},
  NewData = Data#data{input = "0"},
  {next_state, mode, NewData, [{reply, From, Reply}]};
handle_event({call, From}, StateContent, _State, Data) ->
  NewData = get_default_data(Data),
  {next_state, waiting_card, NewData, [{reply, From, StateContent}]}.
