%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(toppage_h).

-include_lib("eunit/include/eunit.hrl").

-ifdef('NO_MAP_TYPE').
-define(OBJ0, {[]}).
-define(OBJ1(K, V), {[{K, V}]}).
-define(OBJ2(K1, V1, K2, V2), {[{K1, V1}, {K2, V2}]}).
-define(OBJECT_FROM_LIST(List), List).
-else.
-define(OBJ0, #{}).
-define(OBJ1(K, V), #{K => V}).
-define(OBJ2(K1, V1, K2, V2), #{K1 => V1, K2 => V2}).
-define(OBJECT_FROM_LIST(List), maps:from_list(List)).
-endif.

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = maybe_echo(Method, HasBody, Req0),
	{ok, Req, Opts}.

maybe_echo(<<"POST">>, true, Req0) ->
	{ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
	ID = proplists:get_value(<<"sensor_id">>, PostVals),
	Data = proplists:get_value(<<"sensor_data">>, PostVals),
	Timestamp = proplists:get_value(<<"timestamp">>, PostVals), % PostVals are in BINARY format
	JsonStart = <<"{\"sensor_id\":\"">>,
	JsonData = <<"\", \"sensor_data\":\"">>,
	JsonTimestamp = <<"\", \"timestamp\":\"">>,
	JsonEnd = <<"\"}">>,
	Body = <<	JsonStart/binary, ID/binary,
						JsonData/binary, Data/binary,
						JsonTimestamp/binary, Timestamp/binary,
						JsonEnd/binary>>,
	echo(Body, Req);

maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req).
