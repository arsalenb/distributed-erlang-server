%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

-import(average_calc_task, [return_avg/1]).
-import(data_log_task, [log_access/4]).


init(Req0, Opts) ->
	io:fwrite("~p~n", ["Message Received..."]),
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = server_request_handler(Method, HasBody, Req0),
	{ok, Req, Opts}.

%% Handler for POST/GET messages
server_request_handler(<<"POST">>, true, Req0) ->
	io:fwrite("~p~n", ["POST Handler..."]),
	{ok, PostContentBin, Req} = cowboy_req:read_urlencoded_body(Req0),
	MsgTypeBin = proplists:get_value(<<"msg_type">>, PostContentBin),
	case binary_to_list(MsgTypeBin) of
		"data_tx" ->
			io:fwrite("~p~n", ["Sensor Data Transmission..."]),
			SensorIDBin = proplists:get_value(<<"sensor_id">>, PostContentBin),
			DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
			TimeBin = proplists:get_value(<<"time">>, PostContentBin),
			%% Store received data
			log_access(write, id001, binary_to_integer(DataBin), binary_to_list(TimeBin)),
			%% Compute mobile average and send reply
			AvgFloat = return_avg(binary_to_integer(DataBin)),
			case is_float(AvgFloat) of
				true ->
					EchoHeader = <<"Regional Server ID: RS001 Echo Data: ">>,
					Body = <<EchoHeader/binary, DataBin/binary>>,
					server_reply(Body, Req),

					AvgBin = float_to_binary(AvgFloat, [{decimals, 4}]),
					if
						AvgFloat > 21 -> threshold_crossed_handler(AvgBin, SensorIDBin, TimeBin);
						true -> th_not_crossed
					end;
				_ -> server_reply(undefined, Req)
			end;
		"request_data" ->
			io:fwrite("~p~n", ["Request Data..."]);

		"th_crossed" ->
			io:fwrite("~p~n", ["Threshold Crossed Message..."]),
			server_reply(<<"ok">>, Req);
			%io:fwrite("~p~n", [binary_to_list(PostContentBin)]);
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
	{DataLog, TimeLog} = log_access(read, id001, [], []),
	io:fwrite("~p~n", [DataLog]),
	io:fwrite("~p~n", [TimeLog]),
	server_reply_html(Body, Req0);

server_request_handler(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

server_request_handler(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

threshold_crossed_handler(AvgBin, SensorIDBin, TimeBin) ->
	io:fwrite("~p~n", ["Sensor Threshold Crossed - Sending Msg to http://localhost:8080"]),
	%Url = "http://localhost:8080",
	%ContentType = "text/plain; charset=utf-8",
	Msg1 = <<"msg_type=th_crossed&sensor_id=">>,
	Msg2 = <<"&sensor_data=">>,
	Msg3 = <<"&time=">>,
	Body = <<Msg1/binary, SensorIDBin/binary, Msg2/binary, AvgBin/binary, Msg3/binary, TimeBin/binary>>,
	io:fwrite("~p~n", [binary_to_list(Body)]).
	%% httpc:request(post, {Url, [], ContentType, Body}, [], []). %% TODO Implement the POST request in separate Task
							%(Method, Request, HttpOptions, Options)
							%Request: {uri_string:uri_string(),[HttpHeader],ContentType :: uri_string:uri_string(),HttpBody},

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

