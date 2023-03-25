%%%-------------------------------------------------------------------
%%% @author brunocasu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2023 17:25
%%%-------------------------------------------------------------------
-module(server_html).
-author("brunocasu").

%% API
-export([build_html_record/4, build_static_html/0]).

-import(data_log_task, [log_access/2]).

build_html_record(SensorIDBin, DataBin, DataTypeBin, TimeBin)->
  End = <<"</p>">>,
  HeadID = <<"<div id=\"special-div\"><p class=\"sensor_id\">">>,
  HtmlSensorID = <<HeadID/binary, SensorIDBin/binary, End/binary>>,

  HeadData = <<"<p>">>,
  HtmlData = <<HeadData/binary, DataBin/binary, End/binary>>,

  HeadType = <<"<p>">>,
  HtmlType = <<HeadType/binary, DataTypeBin/binary, End/binary>>,

  HeadTime = <<"<p class=\"time\">">>,
  HtmlTime = <<HeadTime/binary, TimeBin/binary, End/binary>>,

  EndHead = <<"</div>">>,
  RecordBin = <<HtmlSensorID/binary, HtmlData/binary, HtmlType/binary, HtmlTime/binary, EndHead/binary>>,
  RecordBin.

build_static_html() ->
  LogList = log_access(read, []),
  LogBin = build_html_log(LogList),
  BodyStart = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REGIONAL MONITORING SERVER</title>
	<style>
		#special-div
		{
   		column-count:4;
   		column-gap:40px;
		}
		.sensor_id
		{
      margin:0;
    }
		h1 {text-align: center;}
		h2 {text-align: center;}
	</style>
</head>
<body>
	<h1>STATIC HTML - REGIONAL MONITORING SERVER</h1>
	<h2>Sensor Log -  Server ID: RS02</h2>
	<div id=\"special-div\">
	<p class=\"sensor_id\">SENSOR ID</p>
	<p>READING</p>
	<p>DATATYPE</p>
	<p>TIMSTAMP</p>
	</div>">>,
  BodyEnd = <<"
</body>
<script>
const collection = document.getElementsByClassName(\"time\");
for (let i = 0; i < collection.length; i++) {
  collection[i].innerHTML=new Date(parseInt(collection[i].innerHTML)).toISOString();
}
</script>
</html>">>,
  Body = <<BodyStart/binary, LogBin/binary, BodyEnd/binary>>,
  Body.

build_html_log([H | T]) ->
  Record = list_to_binary(H),
  %%Head = <<"<p>">>,
  %%End = <<"</p>">>,
  %%HtmlLog = <<Head/binary, Record/binary, End/binary>>,
  build_html_log(T, Record).

build_html_log([H | T], HtmlLog) ->
  Record = list_to_binary(H),
  %%Head = <<"<p>">>,
  %%End = <<"</p>">>,
  %%HtmlLogItem = <<Head/binary, Record/binary, End/binary>>,
  HtmlLogUpdate = <<HtmlLog/binary, Record/binary>>,
  build_html_log(T, HtmlLogUpdate);

build_html_log([], HtmlLog) ->
  HtmlLog.