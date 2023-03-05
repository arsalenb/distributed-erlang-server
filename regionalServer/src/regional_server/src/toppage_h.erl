%%%-------------------------------------------------------------------
%%% @author brunocasu

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

%%-import(event_handler_task, [event_handler/2]).
%%-import(msg_formatting, [build_html_data_table/0]).

%% Called when cowboy listener receives a message
init(Req0, Opts) ->
	io:fwrite("~p~n", ["Message Received..."]),
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = server_request_handler(Method, HasBody, Req0), %% handles the request
	{ok, Req, Opts}.

%% Handler for POST/GET messages
server_request_handler(<<"POST">>, true, Req0) ->
	io:fwrite("~p~n", ["POST Handler..."]),
	{ok, PostContentBin, Req} = cowboy_req:read_urlencoded_body(Req0),
	server_reply(<<"Regional Server Echo">>, Req),
	SERVER_ID = <<"RSXX">>, %% DEFAULT VALUE
	SensorIDBin = proplists:get_value(<<"sensor_id">>, PostContentBin),
	DataBin = proplists:get_value(<<"sensor_data">>, PostContentBin),
	DataTypeBin = proplists:get_value(<<"sensor_data_type">>, PostContentBin),
	TimeBin = proplists:get_value(<<"time">>, PostContentBin),
	%% Forward Data to websocket - PID at receiver is data_comm
	CENTRAL_SERVER_NODE = central_server@central,
	{data_comm, CENTRAL_SERVER_NODE} ! {{SERVER_ID, SensorIDBin, DataBin, DataTypeBin, TimeBin}, self()};
	%% event_handler(write_data, PostContentBin); %% Removed event handler

server_request_handler(<<"GET">>, _, Req0) ->
	io:fwrite("~p~n", ["GET Handler..."]),
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

build_static_html() ->
<<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REGIONAL MONITORING SERVER</title>
		<style>
	h1 {text-align: center;}
	h2 {text-align: center;}
	</style>
</head>
<body>
	<h1>STATIC HTML - REGIONAL MONITORING SERVER</h1>">>.