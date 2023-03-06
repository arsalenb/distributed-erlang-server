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
-export([start_monitoring_listener/1,
  log_access/2,
  handle/2,
  send_all/2,
  loop_task/3,
  start_task/3,
  rpc_task/2,
  append_list/2]).

-import(event_handler_task, [event_handler/2]).

%% Spawn the process for the listener task - this should get messages from the regional nodes
start_monitoring_listener(WSPID) ->
  %% IMPORTANT: data_comm is the PID for the regional servers to send the message
  %% check for registration
  List = registered(),
  Status = reg_check(List),
  WsPidLst = pid_to_list(WSPID),
  if
    Status == clear ->
      io:fwrite("~p~n", ["data_comm init..."]),
      register(data_comm, spawn(fun()->loop() end)), %% process that listens to incoming erlang messages
      start_task(log, listener_task, WsPidLst); %% process that keeps the log of websockets PID
    true ->
      log_access(write, WsPidLst),
      already_running
  end.

loop() ->
  receive
    {Msg, _From} ->
      PIDList = log_access(read, []),
      io:fwrite("~p~n", ["sending to websocket info..."]),
      send_all(PIDList, Msg), %% message is sent to all websocket PIDs (connected nodes)
      loop()
  end.

reg_check([H | T]) ->
  if
    H == data_comm -> registered;
    true -> reg_check(T)
  end;

reg_check([]) -> clear.

send_all([H | T], Msg) ->
  PID = list_to_pid(H),
  PID ! Msg,
  send_all(T, Msg);

send_all([], _) -> ok.

log_access(Mode, PID) ->
  rpc_task(log, {Mode, PID}).

handle({write, PID}, List) ->
  if
    length(List) < 500 -> UpdatedList = append_list(List, PID), {log_saved, UpdatedList};
    true -> {log_saved, List}
  end;

handle({read, _}, List) ->
  {List, List}.

%% Spawn the process for the tasks in the server
start_task(LoopID, Module, WsPidLst) ->
 	register(LoopID, spawn(fun()->loop_task(LoopID, Module, [WsPidLst]) end)).

rpc_task(LoopID, Req) ->
	LoopID ! {self(), Req},
	receive {LoopID, Response} -> Response
	end.

loop_task(LoopID, Module, State) ->
	receive
		{From, Req} ->
			{Response, NewState} = Module:handle(Req, State),
			From ! {LoopID, Response},
			loop_task(LoopID, Module, NewState)
	end.

append_list([H | T], L) ->
  [H | T] ++ [L].