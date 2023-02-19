%%%-------------------------------------------------------------------
%%% @author brunocasu

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
			case SensorIDBin of
				<<"001">> -> log_access(write, id001, binary_to_integer(DataBin), binary_to_list(TimeBin));
				<<"002">> -> log_access(write, id002, binary_to_integer(DataBin), binary_to_list(TimeBin));
				_ -> unidentified_id
			end,
			%% Compute mobile average of all sensors in the region and send echo reply
			AvgFloat = return_avg(binary_to_integer(DataBin)),
			case is_float(AvgFloat) of
				true ->
					EchoHeader = <<"Regional Server ID: RS001 Echo Data: ">>,
					Body = <<EchoHeader/binary, DataBin/binary>>,
					server_reply(Body, Req),
					AvgBin = float_to_binary(AvgFloat, [{decimals, 4}]),
					if %% Check if the threshold value was crossed with the new data received
						AvgFloat > 21 -> threshold_crossed_handler(AvgBin, SensorIDBin, TimeBin);
						true -> th_not_crossed
					end;
				_ -> server_reply(undefined, Req)
			end;
		"request_data" ->
			io:fwrite("~p~n", ["Request Data..."]);

		"th_crossed" ->
			io:fwrite("~p~n", ["Threshold Crossed Message..."]),
			server_reply(<<"Threshold Coressed Echo">>, Req);
		_ -> server_reply(undefined, Req)
	end;

server_request_handler(<<"GET">>, _, Req0) ->
	io:fwrite("~p~n", ["GET Handler..."]),
	Body = build_html_data_table(),
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

%<p>REST Regional Server HTML - GET response</p>
build_html_data_table() ->
	BodyTitle = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>RS001 TOP PAGE</title>
		<style>
	h1 {text-align: center;}
	h2 {text-align: center;}
	h3 {text-align: center;}
	p {text-align: center;}
	</style>
</head>
<body>">>,
	BodyEnd = <<"
</body>
</html>">>,
	Header =
		<<"<h1>REGIONAL MONITORING SERVER - ID: RS001</h1>
				<h2>REGION TEMPERATURE (AVERAGE):
		">>,
	AvgFloat = return_avg(read),
	AvgList = float_to_list(AvgFloat, [{decimals, 2}]),
	io:fwrite("~p~n", ["Average:"]),
	io:fwrite("~p~n", [AvgList]),
	AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
	Unit = <<"<span>&#176;</span>C</h2>">>,
	HeaderWithAvg = <<Header/binary, AvgBin/binary, Unit/binary>>,
	TableHeader = <<"<h3>SENSOR 001	DATA LOG
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	SENSOR 002 DATA LOG</h3>">>,
	{DataLog1, TimeLog1} = log_access(read, id001, [], []),
	{DataLog2, TimeLog2} = log_access(read, id002, [], []),
	Table = build_data_table(reverse(DataLog1), reverse(TimeLog1), reverse(DataLog2), reverse(TimeLog2)),
	Body = <<BodyTitle/binary, HeaderWithAvg/binary, TableHeader/binary, Table/binary, BodyEnd/binary>>,
	Body.

build_data_table([D1H | D1T], [T1H | T1T], [D2H | D2T], [T2H | T2T]) ->
	Front = <<"<p>">>,
	Mid = <<"<span>&#176;</span>C
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;">>,
	End = <<"<span>&#176;</span>C</p>">>,
	Data1Bin = integer_to_binary(D1H),
	Time1Bin = list_to_binary(T1H),
	Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;">>,
	Combined1 = <<Time1Bin/binary, Space/binary, Data1Bin/binary>>,
	Data2Bin = integer_to_binary(D2H),
	Time2Bin = list_to_binary(T2H),
	Combined2 = <<Time2Bin/binary, Space/binary, Data2Bin/binary>>,
	Line = <<Front/binary, Combined1/binary, Mid/binary, Combined2/binary, End/binary>>,
	build_data_table(D1T, T1T, D2T, T2T, Line).

build_data_table([D1H | D1T], [T1H | T1T], [D2H | D2T], [T2H | T2T], TableBin) ->
	Front = <<"<p>">>,
	Mid = <<"<span>&#176;</span>C
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;">>,
	End = <<"<span>&#176;</span>C</p>">>,
	Data1Bin = integer_to_binary(D1H),
	Time1Bin = list_to_binary(T1H),
	Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;">>,
	Combined1 = <<Time1Bin/binary, Space/binary, Data1Bin/binary>>,
	Data2Bin = integer_to_binary(D2H),
	Time2Bin = list_to_binary(T2H),
	Combined2 = <<Time2Bin/binary, Space/binary, Data2Bin/binary>>,
	NewTable = <<TableBin/binary, Front/binary, Combined1/binary, Mid/binary, Combined2/binary, End/binary>>,
	build_data_table(D1T, T1T, D2T, T2T, NewTable);
build_data_table([], [], [], [], TableBin) ->
	TableBin.

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].