%%%-------------------------------------------------------------------
%%% @author chur
%%%-------------------------------------------------------------------
-module(sender).
-author("chur").

%% API

-export([send_message/0]).

send_message() ->
  %% Generate a message in the format 'sensor_id=ID&sensor_data=DATA&sensor_data_type=TYPE&time=TIME'
  Message = {<<"RS01">>, <<"TS01">>, <<"20">>, <<"temperature">>, <<"1450879900184">>},
  {interface, erl_comm_interface@localhost} ! {Message, self()}.

%%  %% Connect to the erl_comm_interface@central node
%%  case net_adm:ping('erl_comm_interface@central') of
%%    pong ->
%%      ok;
%%    pang ->
%%      io:format("Error: unable to connect to node erl_comm_interface@central.~n"),
%%      return
%%  end,
%%
%%
%%  %% Send the message to the erl_interface module
%%  {ok, Socket} = gen_tcp:connect("localhost", 0, []),
%%  gen_tcp:send(Socket, Message),
%%
%%  %% Wait for a response from the erl_interface module
%%  receive
%%    {Response, _From} ->
%%      io:format("Received response: ~p~n", [Response])
%%  after
%%    5000 ->
%%      gen_tcp:close(Socket),
%%      net_kernel:connect(erl_comm_interface@central)
%%  end.

