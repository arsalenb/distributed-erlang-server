-module(erl_interface).
-author("brunocasu").

-export([start_listener/1, loop/1, retry_send/4, validate_message/1]).

-define(RETRY_INTERVAL, 1000).
-define(MAX_RETRIES, 5).
-define(TIMEOUT, 60000).

start_listener(PID) ->
  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
  io:fwrite("~p~n", ["erlang interface listener init..."]),
  register(interface, spawn(fun()->loop(PID) end)).

loop(PID) ->
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
  io:fwrite("Received message: ~p~n", [PostContentBin]),

  {ServerID, SensorIDBin, DataBin, DataTypeBin, TimeBin} = PostContentBin,

  ServerIDValid = case ServerID of
                    <<"RS01">> -> true;
                    <<"RS02">> -> true;
                    _ -> false
                  end,

  SensorIDValid = case SensorIDBin of
                    <<"TS01">> -> true;
                    <<"HS01">> -> true;
                    <<"TA01">> -> true;
                    <<"HA01">> -> true;
                    <<"TS02">> -> true;
                    <<"HS02">> -> true;
                    <<"TB02">> -> true;
                    <<"HB02">> -> true;
                    _ -> false
                  end,

  DataValid = case DataTypeBin of
                <<"temperature">> ->
                  case binary_to_integer(DataBin) of
                    N when is_integer(N) -> true;
                    _ -> false
                  end;
                <<"humidity">> ->
                  case binary_to_integer(DataBin) of
                    N when is_integer(N) -> true;
                    _ -> false
                  end;
                _ -> false
              end,

  TimeValid = case binary_to_integer(TimeBin) of
                  X when X > 0 -> true;
                  _ -> false
                end,

  ServerIDValid and SensorIDValid and DataValid and TimeValid.



%%start_listener(PID) ->
%%  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
%%  io:fwrite("~p~n", ["erlang interface listener init..."]),
%%  register(interface, spawn(fun()->loop(PID) end)).
%%
%%loop(PID) ->
%%  receive
%%    {Msg, _From} ->
%%      io:fwrite("~p~n", ["sending to central server listener..."]),
%%      {data_comm, central_server@Distributed2022 } ! {Msg, self()},%% central_server@Distributed2022
%%      loop(PID)
%%  end.