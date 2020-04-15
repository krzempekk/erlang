%%%-------------------------------------------------------------------
%%% @author Kamil Krzempek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2020 09:25
%%%-------------------------------------------------------------------
-module(lab2_qsort).
-author("Kamil Krzempek").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3, map/2, filter/2, digitsSum/1]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) -> [trunc(random:uniform() * (Max - Min + 1) + Min) || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  { Fun1Time, _ } = timer:tc(Fun1, [List]),
  { Fun2Time, _ } = timer:tc(Fun2, [List]),
  io:format("Fun1 time: ~wms~nFun2 time: ~wms~n", [Fun1Time / 1000, Fun2Time / 1000]).

map(_, []) -> [];
map(Pred, [H | T]) -> [Pred(H) | map(Pred, T)].

filter(_, []) -> [];
filter(Pred, [H | T]) ->
  case Pred(H) of
    true -> [H] ++ filter(Pred, T);
    _ -> filter(Pred, T)
  end.

digitsSum(X) -> lists:foldl(fun(X, Acc) -> (X - 48) + Acc end, 0, integer_to_list(X)).