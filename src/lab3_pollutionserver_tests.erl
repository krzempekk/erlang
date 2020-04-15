%%%-------------------------------------------------------------------
%%% @author sans
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 19:50
%%%-------------------------------------------------------------------
-module(lab3_pollutionserver_tests).
-author("sans").
-include_lib("eunit/include/eunit.hrl").

-import(lab3_pollutionserver, [start/0, stop/0, getMonitor/0, addStation/2]).

%% API
-export([]).

prepareServer() -> start().

stopServer(_) -> stop().

runTests(_) -> [
  ?_assert(lists:member(parent, registered())),
  ?_assert(lists:member(pollutionServer, registered())),
  ?_assertEqual(ok, addStation("S1", {1,1}))
].

functions_test_() ->
  {
    foreach,
    local,
    fun prepareServer/0,
    fun stopServer/1,
    [
      fun runTests/1
    ]
  }.