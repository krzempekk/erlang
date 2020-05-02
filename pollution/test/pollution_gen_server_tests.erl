%%%-------------------------------------------------------------------
%%% @author sans
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 19:50
%%%-------------------------------------------------------------------
-module(pollution_gen_server_tests).
-author("sans").
-include_lib("eunit/include/eunit.hrl").

-import(pollution_gen_server, [start/0, stop/0, getMonitor/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyMaxValue/2, getDailyValueCount/2]).

%% API
-export([]).

prepareServer() -> start().

stopServer(_) -> stop().

runTests(_) -> [
  ?_assertEqual(ok, addStation("S1", {1,1})),
  ?_assertEqual(ok, addValue("S1", {{2020,12,1},{10,0,0}}, "PM10", 10)),
  ?_assertEqual(10, getOneValue("S1", {{2020,12,1},{10,0,0}}, "PM10")),
  ?_assertMatch({error, _}, getOneValue("S123", {{2020,12,1},{10,0,0}}, "PM10")),
  ?_assert(10 == getStationMean("S1", "PM10")),
  ?_assertMatch({error, _}, getStationMean("S123", "PM10")),
  ?_assert(10 == getDailyMean({2020,12,1}, "PM10")),
  ?_assertMatch({error, _}, getDailyMean({2020,12,1}, "PM123")),
  ?_assertEqual({{1,1}, 10}, getDailyMaxValue({2020, 12, 1}, "PM10")),
  ?_assertMatch({error, _}, getDailyMaxValue({2020, 12, 1}, "PM123")),
  ?_assertEqual(1, getDailyValueCount({2020, 12, 1}, "PM10")),
  ?_assertMatch(0, getDailyValueCount({2020, 12, 1}, "PM123")),
  ?_assertEqual(ok, removeValue("S1", {{2020,12,1},{10,0,0}}, "PM10"))
].

functions_test_() ->
  {
    setup,
    local,
    fun prepareServer/0,
    fun stopServer/1,
    fun runTests/1
  }.