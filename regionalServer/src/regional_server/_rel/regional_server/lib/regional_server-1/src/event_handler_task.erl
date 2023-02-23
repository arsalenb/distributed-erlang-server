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

-import(regional_server_app, [rpc_task/2]).
-import(average_calc_task, [return_avg/1]).
-import(data_log_task, [log_access/4]).
-import(post_request_task, [post_msg/4]).


init() -> ["Monitoring Start: No Warnings - Date/Time: yyyy-mm-ddThh:mm:ss XX C"].

event_handler(Mode, PostContentBin) ->
  rpc_task(event, {Mode, PostContentBin}).

%% The write_data mode is used when new data is received by the server, and need to be handled
handle({write_data, PostContentBin}, EventList) ->
  SensorIDBin = proplists:get_value(<<"sensor_id">>, PostContentBin),
  DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
  TimeBin = proplists:get_value(<<"time">>, PostContentBin),
  %% Store received data
  case SensorIDBin of
    <<"001">> -> log_access(write, id001, binary_to_integer(DataBin), binary_to_list(TimeBin));
    <<"002">> -> log_access(write, id002, binary_to_integer(DataBin), binary_to_list(TimeBin));
    _ -> unidentified_id
  end,
  AvgFloat = return_avg(binary_to_integer(DataBin)), %% Add new entry to Average calculation list
  io:fwrite("~p~n", ["Average Temp:"]),
  io:fwrite("~p~n", [float_to_list(AvgFloat, [{decimals, 2}])]),
  if %% Data check
    AvgFloat > 21 -> %% Upper Temp Threshold Crossed
      AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
      io:fwrite("~p~n", ["Upper Temp Threshold Crossed..."]),
      WarningHeader = <<"RS001 Warning! UPPER_TS Crossed at: ">>,
      Space = <<" / Avg: ">>,
      Unit = <<" C - Received From Sensor: ">>,
      Reading = <<" - Reading: ">>,
      End = <<" C">>,
      EventRecordBin = <<WarningHeader/binary, TimeBin/binary, Space/binary,
        AvgBin/binary, Unit/binary, SensorIDBin/binary, Reading/binary, DataBin/binary, End/binary>>,
      NewEventList = append_list(EventList, binary_to_list(EventRecordBin)),
      post_msg(<<"RS001">>, <<"UPPER_TS">>, AvgBin, TimeBin),
      {ok, NewEventList}; %% TODO Implement the POST request to RS002
    AvgFloat < 11 -> %% Lower Temp Threshold Crossed
      AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
      io:fwrite("~p~n", ["Lower Temp Threshold Crossed..."]),
      WarningHeader = <<"RS001 Warning! LOWER_TS Crossed at: ">>,
      Space = <<" / Avg: ">>,
      Unit = <<" C - Received From Sensor: ">>,
      Reading = <<" - Reading: ">>,
      End = <<" C">>,
      EventRecordBin = <<WarningHeader/binary, TimeBin/binary, Space/binary,
        AvgBin/binary, Unit/binary, SensorIDBin/binary, Reading/binary, DataBin/binary, End/binary>>,
      NewEventList = append_list(EventList, binary_to_list(EventRecordBin)),
      AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
      post_msg(<<"RS001">>, <<"LOWER_TS">>, AvgBin, TimeBin),
      {ok, NewEventList}; %% TODO Implement the POST request to RS002
    true -> {no_event, EventList}
  end;


%% The write_event mode is used when an event message is received by the server, and need to be stored in the records
handle({write_event, PostContentBin}, EventList) ->
  ServerIDBin = proplists:get_value(<<"server_id">>, PostContentBin),
  EventBin = proplists:get_value(<<"event">>, PostContentBin),
  DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
  TimeBin = proplists:get_value(<<"time">>, PostContentBin),
  io:fwrite("~p~n", [binary_to_list(ServerIDBin)]),
  io:fwrite("~p~n", [binary_to_list(EventBin)]),
  WarningHeader = <<"&nbsp;Warning!&nbsp;">>,
  WarningType = <<"&nbsp;Crossed - Date/Time:&nbsp;">>,
  Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;Temp:&nbsp;">>,
  WarningEnd = <<"<span>&#176;</span>C">>,
  EventRecordBin = <<WarningHeader/binary, EventBin/binary, WarningType/binary,
  TimeBin/binary, Space/binary, DataBin/binary, WarningEnd/binary>>,
  if
    length(EventList) < 50 ->
      NewEventList = append_list(EventList, binary_to_list(EventRecordBin)),
      {ok, NewEventList};
    true ->
      NewEventList = append_list_remove_head(EventList, binary_to_list(EventRecordBin)),
      {ok, NewEventList}
  end;


handle({read, []}, EventList) ->
  {EventList, EventList}.

append_list_remove_head([_H | T], L) ->
  T ++ [L].
append_list([H | T], L) ->
  [H | T] ++ [L].