-module(erl_interface).
-author("brunocasu").

%% API
-export([start_listener/1, loop/1]).


start_listener(PID) ->
  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
  io:fwrite("~p~n", ["erlang interface listener init..."]),
  register(interface, spawn(fun()->loop(PID) end)).

loop(PID) ->
  receive
    {Msg, _From} ->
      io:fwrite("~p~n", ["sending to central server listener..."]),
      {data_comm, central_server@Distributed2022} ! {Msg, self()},
      loop(PID)
  end.
