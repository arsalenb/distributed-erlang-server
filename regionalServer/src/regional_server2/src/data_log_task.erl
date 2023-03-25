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
-export([init/0, handle/2, log_access/2]).

-import(regional_server2_app, [rpc_task/2]).

%% The Log handler will store the last 1000 sensor readings sent to the Regional Server
init() -> ["<p>data record start string - recent records are shown first</p>"]. %% initial condition

log_access(Mode, RecordBin) ->
  rpc_task(log, {Mode, RecordBin}).

%% The data log Task will handle incoming data from the Sensors
%% Every new data entry, the handle function will append the data at the end of the Log List
handle({write, RecordBin}, Log) ->
  Record = binary_to_list(RecordBin),
  MAX_LOG_SIZE = 1000,
  if
    length(Log) < MAX_LOG_SIZE -> %% Maximum size of Record is 1000 readings
      UpdatedLog = append_list(Log, Record),
      {log_saved, UpdatedLog};
    true ->
      UpdatedLog = append_list_remove_head(Log, Record),
      {log_saved, UpdatedLog}
  end;

handle({read, []}, Log) ->
  FIFOLog = reverse(Log),
  {FIFOLog, Log}. %% Return current Log list

append_list_remove_head([_H | T], L) ->
  T ++ [L].
append_list([H | T], L) ->
  [H | T] ++ [L].

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].