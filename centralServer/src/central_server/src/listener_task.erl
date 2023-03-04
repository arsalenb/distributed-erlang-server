%%%-------------------------------------------------------------------
%%% @author brunocasu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2023 22:55
%%%-------------------------------------------------------------------
-module(listener_task).
-author("brunocasu").

%% API
-export([start_monitoring_listener/2]).

-import(event_handler_task, [event_handler/2]).

%% Spawn the process for the listener task - this should get messages from the regional nodes
start_monitoring_listener(Module, WSPID) ->
  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
  io:fwrite("~p~n", ["data_comm init..."]),
  register(data_comm, spawn(fun()->loop(data_comm, Module, WSPID) end)).

loop(LoopID, Module, WSPID) ->
  receive
    {Msg, _From} ->
      WSPID ! Msg, %% message is sent to Websocket PID
      loop(LoopID, Module, WSPID)
  end.
