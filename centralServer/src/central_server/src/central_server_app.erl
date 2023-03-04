%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(central_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/central_server", ws_h, []}
		]}
	]),																%% DEFAULT IP AND PORT
	{ok, _} = cowboy:start_clear(http, [{ip, {127,0,1,1}},{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	central_server_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).
