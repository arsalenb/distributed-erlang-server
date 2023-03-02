%%%-------------------------------------------------------------------
%%% @author brunocasu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Feb 2023 19:57
%%%-------------------------------------------------------------------
-module(event_handler_task).
-author("brunocasu").

%% API
-export([init/0, handle/2, event_handler/2]).

-import(regional_server1_app, [rpc_task/2]).
-import(average_calc_task, [return_avg/1]).
-import(data_log_task, [log_access/4]).
-import(msg_formatting, [build_event_record/5]).


init() -> ["Monitoring Start: No Warnings - Date/Time: yyyy-mm-ddThh:mm:ss XX C"].

event_handler(Mode, PostContentBin) ->
  rpc_task(event, {Mode, PostContentBin}).

%% The write_data mode is used when new data is received by the server, and need to be handled
handle({write_data, PostContentBin}, EventList) ->
  %% Hardcoded Values:
  UPPER_TS_VAL = 21,
  LOWER_TS_VAL = 11,
  SENSOR_ID1 = <<"001">>,
  SENSOR_ID2 = <<"002">>,
  %% Retrieving the information in the POST message sent by the sensor node
  SensorIDBin = proplists:get_value(<<"sensor_id">>, PostContentBin),
  DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
  TimeBin = proplists:get_value(<<"time">>, PostContentBin),
%% Forward received Data to webserver
  {central_shell, central_server@localhost} ! {binary_to_integer(DataBin), self()},
%% Store received data and timestamp in the Log
  case SensorIDBin of
    SENSOR_ID1 -> log_access(write, log1, binary_to_integer(DataBin), binary_to_list(TimeBin));
    SENSOR_ID2 -> log_access(write, log2, binary_to_integer(DataBin), binary_to_list(TimeBin));
    _ -> unidentified_id
  end,
%% Add new entry to Average calculation list
  AvgFloat = return_avg(binary_to_integer(DataBin)),
  io:fwrite("~p~n", ["Average Temp:"]),
  io:fwrite("~p~n", [float_to_list(AvgFloat, [{decimals, 2}])]),
%% Data check - if a threshold is crossed, a new entry to the Event list is added
  AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
  if
    AvgFloat > UPPER_TS_VAL -> %% Upper Temp Threshold Crossed
      io:fwrite("~p~n", ["Upper Temp Threshold Crossed..."]),
      EventRecord = build_event_record(upts, TimeBin, AvgBin, SensorIDBin, DataBin),
      NewEventList = append_list(EventList, EventRecord),
      {ok, NewEventList};
    AvgFloat < LOWER_TS_VAL -> %% Lower Temp Threshold Crossed
      io:fwrite("~p~n", ["Lower Temp Threshold Crossed..."]),
      EventRecord = build_event_record(lwts, TimeBin, AvgBin, SensorIDBin, DataBin),
      NewEventList = append_list(EventList, EventRecord),
      AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
      {ok, NewEventList};
    true -> {no_event, EventList}
  end;

%% The write_event mode is used when an event message is received by the server, and need to be stored in the records
handle({write_event, Content}, EventList) ->
  MAX_LOG_SIZE = 50,
  if
    length(EventList) < MAX_LOG_SIZE ->
      NewEventList = append_list(EventList, Content),
      {ok, NewEventList};
    true ->
      NewEventList = append_list_remove_head(EventList, Content),
      {ok, NewEventList}
  end;


handle({read, []}, EventList) ->
  {EventList, EventList}.

append_list_remove_head([_H | T], L) ->
  T ++ [L].
append_list([H | T], L) ->
  [H | T] ++ [L].