%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

-import(average_calc_task, [return_avg/1]).


init(Req0, Opts) ->
	io:fwrite("~p~n", ["Message Received..."]),
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = server_request_handler(Method, HasBody, Req0),
	{ok, Req, Opts}.

%% Handler for POST/GET messages
server_request_handler(<<"POST">>, true, Req0) ->
	io:fwrite("~p~n", ["POST Handler..."]),
	{ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
	DataBin = proplists:get_value(<<"sensor_data">>, PostVals),
	AvgFloat = return_avg(binary_to_integer(DataBin)),
	case is_float(AvgFloat) of
		true ->
			Echo = float_to_binary(AvgFloat, [{decimals, 4}]),
			server_reply(Echo, Req);
		_ -> server_reply(undefined, Req)
	end;


server_request_handler(<<"GET">>, _, Req0) ->
	io:fwrite("~p~n", ["GET Handler..."]),
	Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Regional Server</title>
</head>
<body>
	<p>REST Regional Server HTML - GET response</p>
</body>
</html>">>,
	server_reply_html(Body, Req0);

server_request_handler(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

server_request_handler(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).


server_reply(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
server_reply(Content, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Content, Req).

server_reply_html(Content, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html; charset=utf-8">>
	}, Content, Req).
