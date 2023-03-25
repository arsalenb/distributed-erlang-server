%%%-------------------------------------------------------------------
%%% @author chur
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 3月 2023 17:16
%%%-------------------------------------------------------------------
-module(receiver).
-author("chur").

%% API
-export([]).

-export([receive_message/0]).

receive_message() ->
  io:fwrite("~p~n", ["receiver init..."]),
  register(data_comm, spawn(fun()->loop() end)).


loop() ->
  receive
    {Msg, _From} ->
      io:fwrite("~p~n", ["receive message..."]),
%%      _From ! {"ok", self()},
      loop()
  end.