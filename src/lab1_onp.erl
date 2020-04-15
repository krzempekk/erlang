%%%-------------------------------------------------------------------
%%% @author Kamil Krzempek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2020 11:50
%%%-------------------------------------------------------------------
-module(lab1_onp).
-author("Kamil Krzempek").

%% API
-export([onp/1]).

%% wyrazenia z cwiczenia
%% 2 3 * 1 + 4 5 / - 6 +
%% 1 2 + 3 + 4 + 5 + 6 7 * +
%% 4 7 + 3 / 2 19 - *
%% 31 4 + 17 * 26 15 - 2 * 22 - / 1 - wyrazenie niepoprawne

onp("") -> 0;
onp(Exp) ->
  Processed = onp_process_floats(onp_process_ints(string:tokens(Exp, " "))),
  onp_parse(Processed, []).

onp_process_ints([]) -> [];
onp_process_ints([H | T]) ->
  try list_to_integer(H) of
    Int -> [Int | onp_process_ints(T)]
  catch
    _:_ -> [H | onp_process_ints(T)]
  end.

onp_process_floats([]) -> [];
onp_process_floats([H | T]) ->
  try list_to_float(H) of
    Float -> [Float | onp_process_floats(T)]
  catch
    _:_ -> [H | onp_process_floats(T)]
  end.

onp_parse([], [A]) -> A;
onp_parse([H | T], Stack) when is_number(H) -> onp_parse(T, [H | Stack]);
onp_parse(["sqrt" | T], [A | Rest]) -> onp_parse(T, [math:sqrt(A) | Rest]);
onp_parse(["sin" | T], [A | Rest]) -> onp_parse(T, [math:sin(A) | Rest]);
onp_parse(["cos" | T], [A | Rest]) -> onp_parse(T, [math:cos(A) | Rest]);
onp_parse(["tan" | T], [A | Rest]) -> onp_parse(T, [math:tan(A) | Rest]);
onp_parse(["+" | T], [B, A | Rest]) -> onp_parse(T, [A + B | Rest]);
onp_parse(["-" | T], [B, A | Rest]) -> onp_parse(T, [A - B | Rest]);
onp_parse(["*" | T], [B, A | Rest]) -> onp_parse(T, [A * B | Rest]);
onp_parse(["/" | T], [B, A | Rest]) -> onp_parse(T, [A / B | Rest]);
onp_parse(["pow" | T], [B, A | Rest]) -> onp_parse(T, [math:pow(A, B) | Rest]);
%% dodatkowe, zmyslone operatory
onp_parse(["inc" | T], [A | Rest]) -> onp_parse(T, [A + 1 | Rest]);
onp_parse(["sq" | T], [B, A | Rest]) -> onp_parse(T, [math:pow(A + B, 2) | Rest]).