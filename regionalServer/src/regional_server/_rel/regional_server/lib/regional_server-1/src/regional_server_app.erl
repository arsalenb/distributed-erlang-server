%%%-------------------------------------------------------------------
%%% @author brunocasu

%% @private
-module(regional_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).
-export([start_task/2]).
-export([rpc_task/2]).
%% API.
%% This function is called by cowboy Makefile
start(_Type, _Args) ->
	start_task(avg, average_calc_task),
	start_task(log, data_log_task),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	regional_server_sup:start_link(). %% Start link function from _sup.erl

stop(_State) ->
	ok = cowboy:stop_listener(http).

%% Spawn the process for the tasks in the server
start_task(LoopID, Module) ->
	register(LoopID, spawn(fun()->loop_task(LoopID, Module, Module:init()) end)).

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
