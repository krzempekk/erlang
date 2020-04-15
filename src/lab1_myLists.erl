%%%-------------------------------------------------------------------
%%% @author sans
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2020 11:39
%%%-------------------------------------------------------------------
-module(lab1_myLists).
-author("sans").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/1]).

contains([], _) -> false;
contains([H | T], Value) ->
  if
    H == Value -> true;
    true -> contains(T, Value)
  end.

contains_2([], _) -> false;
contains_2([H | T], H) -> true;
contains_2([_ | T], X) -> contains_2(T, X).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0;
sumFloats([H | T]) when is_float(H) -> H + sumFloats(T).

sumFloatsTail([], Sum) -> Sum;
sumFloatsTail([H | T], Sum) when is_float(H) -> sumFloatsTail(T, Sum + H).

sumFloatsTail(L) -> sumFloatsTail(L, 0).