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
  if %% Data check
    AvgFloat > 21 -> %% Upper Temp Threshold Crossed
      WarningHeader = <<"Warning! UPPER Temp. Threshold Crossed at RS001 - Date/Time:&nbsp;">>,
      Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;">>,
      WarningEnd = <<"<span>&#176;</span>C">>,
      EventRecordBin = <<WarningHeader/binary, TimeBin/binary, Space/binary, DataBin/binary, WarningEnd/binary>>,
      if
        length(EventList) < 50 -> append_list(EventList, binary_to_list(EventRecordBin));
        true -> append_list_remove_head(EventList, binary_to_list(EventRecordBin))
      end,
      AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
      post_msg(<<"RS001">>, <<"UPTS">>, AvgBin, TimeBin); %% TODO Implement the POST request to RS002
    AvgFloat < 11 -> %% Lower Temp Threshold Crossed
      WarningHeader = <<"Warning! LOWER Temp. Threshold Crossed at RS001 - Date/Time:&nbsp;">>,
      Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;">>,
      WarningEnd = <<"<span>&#176;</span>C">>,
      EventRecordBin = <<WarningHeader/binary, TimeBin/binary, Space/binary, DataBin/binary, WarningEnd/binary>>,
      if
        length(EventList) < 50 -> append_list(EventList, binary_to_list(EventRecordBin));
        true -> append_list_remove_head(EventList, binary_to_list(EventRecordBin))
      end,
      AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
      post_msg(<<"RS001">>, <<"LWTS">>, AvgBin, TimeBin); %% TODO Implement the POST request to RS002
    true -> no_event
  end,
  {ok, EventList};

%% The write_event mode is used when an event message is received by the server, and need to be stored in the records
handle({write_event, PostContentBin}, EventList) ->
ServerIDBin = proplists:get_value(<<"server_id">>, PostContentBin),
EventBin = proplists:get_value(<<"event">>, PostContentBin),
DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
TimeBin = proplists:get_value(<<"time">>, PostContentBin),


append_list_remove_head([_H | T], L) ->
  T ++ [L].
append_list([H | T], L) ->
  [H | T] ++ [L].