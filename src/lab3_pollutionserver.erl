%%%-------------------------------------------------------------------
%%% @author sans
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 16:52
%%%-------------------------------------------------------------------
-module(lab3_pollutionserver).
-author("sans").

%%-import(lab2_pollution, [createMonitor/0, addValue/5, addStation/3, removeValue/4, getDailyValueCount/3, getStationMean/3, getDailyMean/3, getOneValue/4, getDailyMaxValue/3]).

%% API
-export([start/0, stop/0, init/0, getMonitor/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyMaxValue/2, getDailyValueCount/2]).

start() ->
  register(parent, self()),
  register(pollutionServer, spawn(lab3_pollutionserver, init, [])).


init() ->
  loop(lab2_pollution:createMonitor()).


loop(Monitor) ->
  receive
    { Function, Args }
      when (Function == addStation) or (Function == addValue) or (Function == removeValue) ->
      NewMonitor = apply(lab2_pollution, Function, Args ++ [Monitor]),
      case NewMonitor of
        { error, Msg } -> parent ! { error, Msg }, loop(Monitor);
        _ -> parent ! ok, loop(NewMonitor)
      end;
    { Function, Args }
      when (Function == getDailyValueCount) or (Function == getStationMean) or (Function == getDailyMean) or (Function == getOneValue) or (Function == getDailyMaxValue) ->
      ReturnVal = apply(lab2_pollution, Function, Args ++ [Monitor]),
      case ReturnVal of
        { error, Msg } -> parent ! { error, Msg };
        _ -> parent ! ReturnVal
      end,
      loop(Monitor);
    getMonitor -> parent ! Monitor, loop(Monitor);
    stop -> unregister(pollutionServer), ok;
    _ -> parent ! { error, unrecognized_command }, loop(Monitor)
  end.


execute(Function, Args) ->
  pollutionServer ! { Function, Args },
  receive
    Data -> Data
  end.

addStation(Name, Coords) -> execute(addStation, [Name, Coords]).

addValue(StationInfo, Date, Type, Value) -> execute(addValue, [StationInfo, Date, Type, Value]).

removeValue(StationInfo, Date, Type) -> execute(removeValue, [StationInfo, Date, Type]).

getOneValue(StationInfo, Date, Type) -> execute(getOneValue, [StationInfo, Date, Type]).

getStationMean(StationInfo, Type) -> execute(getStationMean, [StationInfo, Type]).

getDailyMean(Date, Type) -> execute(getDailyMean, [Date, Type]).

getDailyMaxValue(Date, Type) -> execute(getDailyMaxValue, [Date, Type]).

getDailyValueCount(Date, Type) -> execute(getDailyValueCount, [Date, Type]).

getMonitor() ->
  pollutionServer ! getMonitor,
  receive
    Monitor -> Monitor
  end.

stop() -> pollutionServer ! stop, unregister(parent).

