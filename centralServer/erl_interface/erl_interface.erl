-module(erl_interface).
-author("brunocasu").

%% API
-export([start_listener/1, loop/1]).

%% Spawn the process for the listener task - this should get messages from the regional nodes
start_listener(PID) ->
  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
  io:fwrite("~p~n", ["erlang interface listener init..."]),
  register(data_comm, spawn(fun()->loop(PID) end)).

loop(PID) ->
  receive
    {Msg, _From} ->
      {data_comm, central_server@brunocasu} ! {Msg, self()},
      loop(PID)
  end.
