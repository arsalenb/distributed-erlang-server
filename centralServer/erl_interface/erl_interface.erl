-module(erl_interface).
-author("brunocasu").

%%start_listener(PID) ->
%%  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
%%  io:fwrite("~p~n", ["erlang interface listener init..."]),
%%  register(interface, spawn(fun()->loop(PID) end)).
%%
%%loop(PID) ->
%%  receive
%%    {Msg, _From} ->
%%      io:fwrite("~p~n", ["sending to central server listener..."]),
%%      {data_comm, central_server@Distributed2022} ! {Msg, self()},
%%      loop(PID)
%%  end.

%% API
%% API
-export([start_listener/1, loop/1, retry_send/4, validate_message/1]).

-define(RETRY_INTERVAL, 1000).
-define(MAX_RETRIES, 5).
-define(TIMEOUT, 60000).

start_listener(PID) ->
  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
  io:fwrite("~p~n", ["erlang interface listener init..."]),
  register(interface, spawn(fun()->loop(PID) end)).

loop(PID) ->
  io:fwrite("inside loop"),
  receive
    {Msg, _From} ->
      case validate_message(Msg) of
        true ->
          retry_send(Msg, ?MAX_RETRIES, ?TIMEOUT, PID);
        false ->
          io:fwrite("~p~n", ["ignoring invalid message..."])
      end,
      loop(PID)
  end.

retry_send(Msg, Retries, Timeout, PID) when Retries > 0, Timeout > 0 ->
  case catch({data_comm, central_server@Distributed2022} ! {Msg, self()}) of
    {'EXIT', _Reason} ->
      timer:sleep(?RETRY_INTERVAL),
      io:fwrite("~p~n", ["Error sending message, retrying..."]),
      retry_send(Msg, Retries - 1, Timeout - ?RETRY_INTERVAL, PID);
    _ ->
      io:fwrite("~p~n", ["sending to central server listener..."])
  end;
retry_send(_Msg, _Retries, 0, _PID) ->
  io:fwrite("~p~n", ["Timeout reached, ignoring message..."]);
retry_send(_Msg, 0, _Timeout, _PID) ->
  io:fwrite("~p~n", ["Exceeded maximum retries, ignoring message..."]).

validate_message(PostContentBin) ->
  SensorIDBin = proplists:get_value(<<"sensor_id">>, PostContentBin),
  DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
  DataTypeBin = proplists:get_value(<<"sensor_data_type">>, PostContentBin),
  TimeBin = proplists:get_value(<<"time">>, PostContentBin),

  SensorIDValid = case SensorIDBin of
                    <<"TS01">> -> true;
                    <<"HS01">> -> true;
                    _ -> false
                  end,

  DataValid = case DataTypeBin of
                <<"temperature">> ->
                  case binary_to_integer(DataBin) of
                    N when N >= 10, N =< 30 -> true;
                    _ -> false
                  end;
                <<"humidity">> ->
                  case binary_to_integer(DataBin) of
                    N when N >= 85, N =< 99 -> true;
                    _ -> false
                  end;
                _ -> false
              end,

  TimeValid = (length(TimeBin) > 0),

  SensorIDValid and DataValid and TimeValid.

