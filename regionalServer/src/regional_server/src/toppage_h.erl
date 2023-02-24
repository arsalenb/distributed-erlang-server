%%%-------------------------------------------------------------------
%%% @author brunocasu

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

-import(event_handler_task, [event_handler/2]).
-import(msg_formatting, [build_html_data_table/0]).

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
	MsgTypeBin = proplists:get_value(<<"msg_type">>, PostContentBin),
	case binary_to_list(MsgTypeBin) of
		"data_tx" -> %% Sent by the sensor nodes - msg contains sensor ID, Data and Timestamp
			io:fwrite("~p~n", ["Sensor Data Received..."]),
			server_reply(<<"Regional Server ID: RS001 Echo">>, Req),
			%% Send data to Event Handler (Save data in the logs, compute the Average Temperature on the region and manage events)
			event_handler(write_data, PostContentBin);

		"event" -> %% Sent by the other monitoring servers - contains source server ID, Type of event, Data value and Timestamp
			io:fwrite("~p~n", ["Event Received..."]),
			server_reply(<<"Regional Server ID: RS001 Echo">>, Req),
			%% Send event to Event Handler (Save the event information in the logs)
			event_handler(write_event, PostContentBin);

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

