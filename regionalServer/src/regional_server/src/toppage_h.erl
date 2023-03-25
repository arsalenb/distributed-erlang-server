%%%-------------------------------------------------------------------
%%% @author brunocasu

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

%%-import(event_handler_task, [event_handler/2]).
%%-import(msg_formatting, [build_html_data_table/0]).
-import(server_html, [build_html_record/4, build_static_html/0]).
-import(data_log_task, [log_access/2]).

%% Called when cowboy listener receives a message
init(Req0, Opts) ->
	io:fwrite("~p~n", ["message received..."]),
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = server_request_handler(Method, HasBody, Req0), %% handles the request
	{ok, Req, Opts}.

%% Handler for POST/GET messages
server_request_handler(<<"POST">>, true, Req0) ->
	io:fwrite("~p~n", ["POST handler..."]),
	{ok, PostContentBin, Req} = cowboy_req:read_urlencoded_body(Req0),
	server_reply(<<"Regional Server Echo">>, Req),
	SERVER_ID = <<"RS01">>, %% SET VALUE FOR EACH REGIONAL SERVER
	SensorIDBin = proplists:get_value(<<"sensor_id">>, PostContentBin),
	io:fwrite("~p~n", ["data from..."]),
	io:fwrite("~p~n", [SensorIDBin]),
	DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
	DataTypeBin = proplists:get_value(<<"sensor_data_type">>, PostContentBin),
	TimeBin = proplists:get_value(<<"time">>, PostContentBin),
	%% TimeListSeconds = binary_to_list(TimeBin, 1, 9),
	%% Store the sensor reading in the Logs
	RecordBin = build_html_record(SensorIDBin, DataBin, DataTypeBin, TimeBin),
	log_access(write, RecordBin),
	%% Forward Data to websocket - PID at receiver is data_comm
	CENTRAL_SERVER_NODE = erl_comm_interface@central,
	{interface, CENTRAL_SERVER_NODE} ! {{SERVER_ID, SensorIDBin, DataBin, DataTypeBin, TimeBin}, self()};

server_request_handler(<<"GET">>, _, Req0) ->
	io:fwrite("~p~n", ["GET handler..."]),
	Body = build_static_html(),
	%% Body = build_html_data_table(), %% Sensor information Removed
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

