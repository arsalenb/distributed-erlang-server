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

-export([init/0, handle/2, return_avg/1]).

-import(regional_server_app, [rpc_avg/2]).

init() -> [15,15,15,15,15].

%% Returns current mobile average
return_avg(Val) ->
    case is_integer(Val) of
        true -> rpc_avg(avg, Val);
        _ -> badarg
    end.


handle(Val, [_H | T]) -> %% Received H is discarded - The Last 5th reading is replaced by the new input value
    NewList = T ++ [Val], %% Add new value to the last 5 Data readings
    io:fwrite("~p~n", ["List For Average Calculation:"]),
    io:fwrite("~w~n", [NewList]),
    SumList = sum(NewList),
    {SumList/5, NewList}. %% Returns mobile average of last 5 Readings

sum(L) ->
    sum(L, 0).

sum([H|T], Acc) ->
    sum(T, H + Acc);

sum([], Acc) ->
    Acc.