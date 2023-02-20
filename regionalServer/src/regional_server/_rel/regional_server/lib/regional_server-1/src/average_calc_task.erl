%%%-------------------------------------------------------------------
%%% @author brunocasu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Feb 2023 17:35
%%%-------------------------------------------------------------------
-module(average_calc_task).
-author("brunocasu").

%% API
-export([init/0, handle/2, return_avg/1]).

-import(regional_server_app, [rpc_task/2]).

init() -> [15,15,15,15,15,15]. %% initial condition

%% Returns mobile average of the 6 last values
return_avg(read) ->
    rpc_task(avg, read);

return_avg(Val) -> %% New value to be used in the Average calculation
    case is_integer(Val) of
        true -> rpc_task(avg, Val);
        _ -> badarg
    end.

handle(read, CurrentList) ->
    SumList = sum(CurrentList),
    if
        SumList/6 > 21 -> {{SumList/6, <<"&nbsp;&nbsp;WARNING! THRESHOLD CROSSED">>}, CurrentList}; %% HTML message
        true -> {{SumList/6, <<"">>}, CurrentList}
    end;

handle(Val, [_H | T]) -> %% Received H is discarded (oldest reading)
    NewList = T ++ [Val], %% Add new value to the last 6 Data readings
    io:fwrite("~p~n", ["List For Average Calculation:"]),
    io:fwrite("~w~n", [NewList]),
    SumList = sum(NewList),
    {SumList/6, NewList}. %% Returns mobile average of last 6 Readings

sum(L) -> %% Sum all values from the input list
    sum(L, 0).

sum([H|T], S) ->
    sum(T, H + S);

sum([], S) ->
    S.