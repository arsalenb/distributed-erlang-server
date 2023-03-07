%%%-------------------------------------------------------------------
%%% @author brunocasu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2023 02:25
%%%-------------------------------------------------------------------
-module(data_log_task).
-author("brunocasu").

%% API
-export([init/0, handle/2, handle_deprecated/2, log_access/4]).

-import(regional_server_app, [rpc_task/2]).

%% 2 Sensors are hardcoded in the Server
%% Sensor Log is made of 2 Tuples: {{[Data1List], [Timestamp1List]}, {[Data2List], [Timestamp2List]}}}
%% Each tuple has the list for one sensor data log and respective timestamp
init() -> {{[0], ["yy-mm-ddThh:mm:ss"]}, {[0], ["yy-mm-ddThh:mm:ss"]}}. %% initial condition

log_access(Mode, ID, Data, Time) ->
  rpc_task(log, {Mode, ID, Data, Time}).

%% The data log Task will handle incoming data from the Sensors
%% Every new data entry, the handle function will append the data at the end of the DataList
handle({write, log1, Data, Time}, {{DataList1, TimeList1}, {DataList2, TimeList2}}) ->
  MAX_LOG_SIZE = 50,
  if
    length(DataList1) < MAX_LOG_SIZE -> %% Maximum size of Record is 50 readings
      UpdatedDataLog = append_list(DataList1, Data),
      UpdatedTimeLog = append_list(TimeList1, Time),
      {log_saved, {{UpdatedDataLog, UpdatedTimeLog}, {DataList2, TimeList2}}};
    true ->
      UpdatedDataLog = append_list_remove_head(DataList1, Data),
      UpdatedTimeLog = append_list_remove_head(TimeList1, Time),
      {log_saved, {{UpdatedDataLog, UpdatedTimeLog}, {DataList2, TimeList2}}}
  end;

handle({write, log2, Data, Time}, {{DataList1, TimeList1}, {DataList2, TimeList2}}) ->
  MAX_LOG_SIZE = 50,
  if
    length(DataList2) < MAX_LOG_SIZE -> %% Maximum size of Record is 50 readings
      UpdatedDataLog = append_list(DataList2, Data),
      UpdatedTimeLog = append_list(TimeList2, Time),
      {log_saved, {{DataList1, TimeList1}, {UpdatedDataLog, UpdatedTimeLog}}};
    true ->
      UpdatedDataLog = append_list_remove_head(DataList2, Data),
      UpdatedTimeLog = append_list_remove_head(TimeList2, Time),
      {log_saved, {{DataList1, TimeList1}, {UpdatedDataLog, UpdatedTimeLog}}}
  end;

handle({read, [], [], []}, {{DataList1, TimeList1}, {DataList2, TimeList2}}) ->
  {{DataList1, TimeList1, DataList2, TimeList2}, {{DataList1, TimeList1}, {DataList2, TimeList2}}}.


%% The data log Task will handle incoming data from the Sensors
%% Every new data entry, the handle function will append the data at the end of the DataList
handle_deprecated({Mode, ID, Data, Time}, {{DataList1, TimeList1}, {DataList2, TimeList2}}) ->
  MAX_LOG_SIZE = 50,
  case Mode of
    write ->
      case ID of
        id001 ->
          if
            length(DataList1) < MAX_LOG_SIZE -> %% Maximum size of Record is 50 readings
              UpdatedDataLog = append_list(DataList1, Data),
              UpdatedTimeLog = append_list(TimeList1, Time),
              {log_saved, {{UpdatedDataLog, UpdatedTimeLog}, {DataList2, TimeList2}}};
            true ->
              UpdatedDataLog = append_list_remove_head(DataList1, Data),
              UpdatedTimeLog = append_list_remove_head(TimeList1, Time),
              {log_saved, {{UpdatedDataLog, UpdatedTimeLog}, {DataList2, TimeList2}}}
          end
        ;
        id002 ->
          if
            length(DataList2) < MAX_LOG_SIZE -> %% Maximum size of Record is 50 readings
              UpdatedDataLog = append_list(DataList2, Data),
              UpdatedTimeLog = append_list(TimeList2, Time),
              {log_saved, {{DataList1, TimeList1}, {UpdatedDataLog, UpdatedTimeLog}}};
            true ->
              UpdatedDataLog = append_list_remove_head(DataList2, Data),
              UpdatedTimeLog = append_list_remove_head(TimeList2, Time),
              {log_saved, {{DataList1, TimeList1}, {UpdatedDataLog, UpdatedTimeLog}}}
          end
        ;
        _ -> {badarg, {{DataList1, TimeList1}, {DataList2, TimeList2}}}
      end
    ;
    read ->
      case ID of
        id001 ->
          {{DataList1, TimeList1}, {{DataList1, TimeList1}, {DataList2, TimeList2}}}
        ;
        id002 ->
          {{DataList2, TimeList2}, {{DataList1, TimeList1}, {DataList2, TimeList2}}}
        ;
        _ -> {badarg, {{DataList1, TimeList1}, {DataList2, TimeList2}}}
      end
    ;
    _ -> {badarg, {{DataList1, TimeList1}, {DataList2, TimeList2}}}
  end.

append_list_remove_head([_H | T], L) ->
  T ++ [L].
append_list([H | T], L) ->
  [H | T] ++ [L].