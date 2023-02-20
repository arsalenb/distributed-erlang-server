%%%-------------------------------------------------------------------
%%% @author brunocasu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Feb 2023 04:52
%%%-------------------------------------------------------------------
-module(msg_formating).
-author("brunocasu").

%% API
-export([build_html_data_table/0, build_json_data_table/0]).

-import(average_calc_task, [return_avg/1]).
-import(data_log_task, [log_access/4]).

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
  {AvgFloat, StatusBin} = return_avg(read),
  AvgList = float_to_list(AvgFloat, [{decimals, 2}]),
  io:fwrite("~p~n", ["Average:"]),
  io:fwrite("~p~n", [AvgList]),
  io:fwrite("~p~n", [binary_to_list(StatusBin)]),
  AvgBin = float_to_binary(AvgFloat, [{decimals, 2}]),
  Unit = <<"<span>&#176;</span>C">>,
  CloseHeader = <<"</h2>">>,
  HeaderWithAvg = <<Header/binary, AvgBin/binary, Unit/binary, StatusBin/binary, CloseHeader/binary>>,
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
build_data_table([], [], [D2H | D2T], [T2H | T2T], TableBin) ->
  Front = <<"<p>">>,
  Mid = <<"
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;">>,
  End = <<"<span>&#176;</span>C</p>">>,
  Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;">>,
  Combined1 = <<"
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;">>,
  Data2Bin = integer_to_binary(D2H),
  Time2Bin = list_to_binary(T2H),
  Combined2 = <<Time2Bin/binary, Space/binary, Data2Bin/binary>>,
  NewTable = <<TableBin/binary, Front/binary, Combined1/binary, Mid/binary, Combined2/binary, End/binary>>,
  build_data_table([], [], D2T, T2T, NewTable);
build_data_table([D1H | D1T], [T1H | T1T], [], [], TableBin) ->
  Front = <<"<p>">>,
  Mid = <<"<span>&#176;</span>C
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;">>,
  End = <<"&nbsp;&nbsp;">>,
  Data1Bin = integer_to_binary(D1H),
  Time1Bin = list_to_binary(T1H),
  Space = <<"&nbsp;&nbsp;&nbsp;&nbsp;">>,
  Combined1 = <<Time1Bin/binary, Space/binary, Data1Bin/binary>>,
  Combined2 = <<"
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
	&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;">>,
  NewTable = <<TableBin/binary, Front/binary, Combined1/binary, Mid/binary, Combined2/binary, End/binary>>,
  build_data_table(D1T, T1T, [], [], NewTable);
build_data_table([], [], [], [], TableBin) ->
  TableBin.

build_json_data_table() -> []. %% TODO

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].