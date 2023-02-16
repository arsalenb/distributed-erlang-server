%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(regional_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).
-export([start_avg_task/2, rpc_avg/2]). %% Avg task handler

%% API.
%% This function is called by cowboy Makefile
start(_Type, _Args) ->
	start_avg_task(avg, average_calc_task),
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

%% The average calculation task will handle incoming data from the sensors
%% returning the average value from the data sent
start_avg_task(LoopID, Module) ->
    io:fwrite("~p~n", ["Start Average Task..."]),
	register(LoopID, spawn(fun()->loop_avg(LoopID, Module, Module:init()) end)).

rpc_avg(LoopID, Req) ->
	LoopID ! {self(), Req},
	receive {LoopID, Response} -> Response
	end.

loop_avg(LoopID, Module, State) ->
	receive
		{From, Req} ->
			{Response, NewState} = Module:handle(Req, State),
			From ! {LoopID, Response},
			loop_avg(LoopID, Module, NewState)
	end.
