%%%-------------------------------------------------------------------
%%% @author brunocasu

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

-import(average_calc_task, [return_avg/1]).
-import(data_log_task, [log_access/4]).
-import(post_request_task, [post_msg/3]).
-import(msg_formating, [build_html_data_table/0, build_json_data_table/0]).


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
						AvgFloat > 21 -> post_msg(<<"RS001">>, AvgBin, TimeBin); %% TODO Implement the POST request in separate Task
						true -> th_not_crossed
					end;
				_ -> server_reply(undefined, Req)
			end;
		"request_data" ->
			Body = build_json_data_table(), %% TODO Implement the POST request response for the Tomcat server - json string
			server_reply(Body, Req),
			io:fwrite("~p~n", ["Request Data..."]);

		"th_crossed" ->
			io:fwrite("~p~n", ["Threshold Crossed Message..."]),
			server_reply(<<"Threshold Crossed Echo">>, Req);
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

