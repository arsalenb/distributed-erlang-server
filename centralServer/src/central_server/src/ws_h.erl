-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-import(listener_task, [start_monitoring_listener/2]).

init(Req, State) ->
	{cowboy_websocket, Req, State, #{
		idle_timeout => 6000000}}. %% 100 min timeout

websocket_init(State) ->
	start_monitoring_listener(listener_task, self()),
	{[], State}.

websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({SensorID, Data, DataType}, State) ->
	Body = build_json_reply(SensorID, Data, DataType),
	{[{text, Body}], State};
websocket_info(_Info, State) ->
	{[], State}.

build_json_reply(SensorID, Data, DataType) ->
	IDKey = <<"{\"sensor_id\":\"">>,
	IDBin = <<IDKey/binary, SensorID/binary>>,
	DataKey = <<"\",\"data\":\"">>,
	DataBin = <<DataKey/binary, Data/binary>>,
	TypeKey = <<"\",\"data_type\":\"">>,
	TypeBin = <<TypeKey/binary, DataType/binary>>,
	End = <<"\"}">>,
	Body = <<IDBin/binary, DataBin/binary, TypeBin/binary, End/binary>>,
	Body.