%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(toppage_h).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	DataList = ["Start"],
	append(DataList, DataList),
	io:fwrite("~w~n", [DataList]),
	Req = server_request_handler(Method, HasBody, Req0, DataList),
	{ok, Req, Opts}.

%% Handler for POST/GET messages
server_request_handler(<<"POST">>, true, Req0, DataList) -> %% POST request handler
	{ok, PostContent, Req} = cowboy_req:read_urlencoded_body(Req0),
	ResponseContent = post_data_handler(PostContent, Req, DataList),
	% Using Erlang inets standard lib to generate the POST request to the secondary server
	%inets:start(),
	%Url = "http://localhost/api/foo/",
	%AuthHeader = {"Authorization", "Bearer abc123"},
	%{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
	%	httpc:request(post,
	%		{Url, [AuthHeader]},
	%		[],
	%		[]),
	%io:format("~s", [Body]),
	server_reply(ResponseContent, Req);

server_request_handler(<<"GET">>, _, Req0, DataList) ->
	Body1 = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Regional Server</title>
</head>
<body>
	<p>REST Regional Server HTML - GET response</p>
	<p>DATA : ">>,
	Val = lists:nth(2, DataList), %% get item from list
	Str = io_lib:format("~.2f", [Val]), %% float to string
	BodyData = list_to_binary(Str),
	Body2 = <<" </p>
</body>
</html>">>,
	Body = <<Body1/binary, BodyData/binary, Body2/binary>>,
	server_reply_html(Body, Req0);

server_request_handler(<<"POST">>, false, Req, _) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

server_request_handler(_, _, Req, _) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).


%% Handler for POST/GET messages
post_data_handler(PostContent, Req, DataList) ->
	ID = proplists:get_value(<<"sensor_id">>, PostContent, not_found),
	post_data_handler(sensor_id, PostContent, ID, Req, DataList). %% Try to get sensor information

post_data_handler(sensor_id, PostContent, not_found, Req, DataList) -> %% Failed to find sensor_id, try server_id
	ID = proplists:get_value(<<"server_id">>, PostContent, not_found),
	post_data_handler(server_id, PostContent, ID, Req, DataList);

post_data_handler(server_id, _, not_found, Req, _) -> %% Failed to find sensor_id and server_id, Content is unidentified
	server_reply(undefined, Req);

post_data_handler(sensor_id, PostContent, ID, _, _DataList) ->
	Data = proplists:get_value(<<"sensor_data">>, PostContent),
	%_FData = binary_to_float(Data),
	%io:fwrite("~.2f", [FData]),
	%append(DataList, io_lib:format("~.2f", [FData])),
	%io:fwrite("~p~n", [DataList]),
	Timestamp = proplists:get_value(<<"timestamp">>, PostContent),
	JsonStart = <<"{\"sensor_id\":\"">>,
	JsonData = <<"\", \"sensor_data\":\"">>,
	JsonTimestamp = <<"\", \"timestamp\":\"">>,
	JsonEnd = <<"\"}">>,
	ResponseContent = <<	JsonStart/binary, ID/binary,
		JsonData/binary, Data/binary,
		JsonTimestamp/binary, Timestamp/binary,
		JsonEnd/binary>>,
	ResponseContent; %% Return binary content

post_data_handler(server_id, PostContent, ID, _, _) ->
	Th = proplists:get_value(<<"threshold">>, PostContent),
	Value = proplists:get_value(<<"value">>, PostContent),
	Timestamp = proplists:get_value(<<"timestamp">>, PostContent),
	JsonStart = <<"{\"server_id\":\"">>,
	JsonTh = <<"\", \"threshold\":\"">>,
	JsonValue = <<"\", \"value\":\"">>,
	JsonTimestamp = <<"\", \"timestamp\":\"">>,
	JsonEnd = <<"\"}">>,
	ResponseContent = <<	JsonStart/binary, ID/binary,
		JsonTh/binary, Th/binary,
		JsonValue/binary, Value/binary,
		JsonTimestamp/binary, Timestamp/binary,
		JsonEnd/binary>>,
	ResponseContent. %% Return binary content


server_reply(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
server_reply(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req).

server_reply_html(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html; charset=utf-8">>
	}, Echo, Req).

append([H|T], Tail) ->
	[H|append(T, Tail)];
append([], Tail) ->
	Tail.