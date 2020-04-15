%%%-------------------------------------------------------------------
%%% @author sans
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 19:53
%%%-------------------------------------------------------------------
-module(lab2_pollution_tests).
-author("sans").
-include_lib("eunit/include/eunit.hrl").

-import(lab2_pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4, getDailyValueCount/3, getDailyMaxValue/3, getDailyMean/3, getStationMean/3, getOneValue/4]).

%% API
-export([]).

prepareMonitor() ->
  M1 = createMonitor(),
  M2 = addStation("S1", {1,1}, M1),
  M3 = addStation("S2", {2,2}, M2),
  M4 = addValue("S1", {{2020,12,1},{10,0,0}}, "PM10", 10, M3),
  M5 = addValue({2,2}, {{2020,12,1},{20,0,0}}, "PM10", 20, M4),
  M6 = addValue({1,1}, {{2020,12,1},{12,0,0}}, "PM10", 30, M5),
  M7 = addValue("S2", {{2020,12,1},{10,0,0}}, "PM2.5", 5, M6),
  M8 = addValue("S1", {{2020,12,3},{10,0,0}}, "PM2.5", 15, M7),
  M9 = addValue({2,2}, {{2020,12,1},{15,0,0}}, "PM10", 40, M8),
  M9.

addInvalidStationTest(M) -> [
    ?_assertEqual({error, station_exist}, addStation("S3", {1,1}, M)),
    ?_assertEqual({error, station_exist}, addStation("S1", {3,3}, M))
].

addInvalidValueTest(M) -> [
  ?_assertEqual({error, value_exist}, addValue("S1", {{2020,12,1},{10,0,0}}, "PM10", 50, M)),
  ?_assertEqual({error, station_dont_exist}, addValue("S10", {{2020,12,1},{10,0,0}}, "PM10", 50, M))
].

removeValueTest(M) ->
  M1 = removeValue({1,1}, {{2020,12,1},{10,0,0}}, "PM10", M),
  [
    ?_assertEqual(10, getOneValue({1,1}, {{2020,12,1},{10,0,0}}, "PM10", M)),
    ?_assertEqual({error, value_dont_exist}, getOneValue({1,1}, {{2020,12,1},{10,0,0}}, "PM10", M1))
  ].

getDailyValueCountTest(M) -> [
  ?_assertEqual(4, getDailyValueCount({2020,12,1}, "PM10", M)),
  ?_assertEqual(0, getDailyValueCount({2020,12,2}, "PM10", M))
].

getDailyMaxValueTest(M) -> [
  ?_assertEqual({{2,2}, 40}, getDailyMaxValue({2020,12,1}, "PM10", M))
].

getDailyMeanTest(M) -> [
  ?_assert(25 == getDailyMean({2020,12,1}, "PM10", M)),
  ?_assertEqual({error, no_matching_values}, getDailyMean({1999,12,1}, "PM10", M))
].

getStationMeanTest(M) -> [
  ?_assert(20 == getStationMean({1,1}, "PM10", M)),
  ?_assertEqual({error, no_matching_values}, getStationMean({1,1}, "PM5", M)),
  ?_assertEqual({error, station_dont_exist}, getStationMean({3,3}, "PM10", M))
].

getOneValueTest(M) -> [
  ?_assertEqual(15, getOneValue({1,1},{{2020,12,3},{10,0,0}},"PM2.5", M)),
  ?_assertEqual({error, value_dont_exist}, getOneValue({1,1},{{1999,12,3},{10,0,0}},"PM2.5", M))
].


functions_test_() ->
  {
    foreach,
    fun prepareMonitor/0,
    [
      fun addInvalidStationTest/1,
      fun addInvalidValueTest/1,
      fun removeValueTest/1,
      fun getDailyValueCountTest/1,
      fun getDailyMaxValueTest/1,
      fun getDailyMeanTest/1,
      fun getStationMeanTest/1,
      fun getOneValueTest/1
    ]
  }.