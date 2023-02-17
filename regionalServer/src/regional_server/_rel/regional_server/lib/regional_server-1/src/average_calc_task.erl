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

init() -> [15,15,15,15,15]. %% initial condition

%% Returns mobile average of the 5 last values
return_avg(Val) ->
    case is_integer(Val) of
        true -> rpc_task(avg, Val);
        _ -> badarg
    end.

handle(Val, [_H | T]) -> %% Received H is discarded - The Last 5th reading is replaced by the new input value
    NewList = T ++ [Val], %% Add new value to the last 5 Data readings
    io:fwrite("~p~n", ["List For Average Calculation:"]),
    io:fwrite("~w~n", [NewList]),
    SumList = sum(NewList),
    {SumList/5, NewList}. %% Returns mobile average of last 5 Readings

sum(L) -> %% Sum all values from the input list
    sum(L, 0).

sum([H|T], S) ->
    sum(T, H + S);

sum([], S) ->
    S.