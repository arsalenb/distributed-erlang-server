%%%-------------------------------------------------------------------
%%% @author chur
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 3月 2023 18:33
%%%-------------------------------------------------------------------
-module(test).
-author("chur").

%% API
-export([pass_message/0]).

pass_message() ->
  {interface, erl_comm_interface@central} ! {{<<"RS01">>, <<"00000">>, <<"20">>, <<"temperature">>, <<"0000000000000">>}, self()}.
