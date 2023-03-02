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
-export([start_monitoring_listener/1, init/0]).

-import(event_handler_task, [event_handler/2]).

%% Spawn the process for the listener task - this should get messages from other regional nodes
start_monitoring_listener(Module) ->
  register(event_comm, spawn(fun()->loop(event_comm, Module, Module:init()) end)).

loop(LoopID, Module, ok) ->
  receive
    {Event, _From} ->
      event_handler(write_event, Event),
      loop(LoopID, Module, ok)
  end.

init() -> ok.