%%%-------------------------------------------------------------------
%%% @author sans
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 19:50
%%%-------------------------------------------------------------------
-module(pollution_sup_tests).
-author("sans").
-include_lib("eunit/include/eunit.hrl").

-import(pollution_sup, [start_link/0]).
-import(pollution_gen_server, [start/0, stop/0]).

%% API
-export([]).

prepare() -> start_link().

stop(_) -> ok.

runTests(_) -> [
  ?_assertMatch({error, _}, start()),
  ?_assertEqual(ok, stop()),
  ?_assertMatch({error, _}, start())
].

functions_test_() ->
  {
    setup,
    local,
    fun prepare/0,
    fun stop/1,
    fun runTests/1
  }.