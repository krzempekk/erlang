%%%-------------------------------------------------------------------
%%% @author Kamil Krzempek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2020 10:23
%%%-------------------------------------------------------------------
-module(pollution).
-author("Kamil Krzempek").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getDailyMean/3, getStationMean/3, getDailyMaxValue/3, getDailyValueCount/3]).

-record(monitor, { stations=[], values=#{} }).

%% createMonitor - działanie funkcji zgodne z instrukcją do labolatoriów

createMonitor() -> #monitor{}.

%% matchingStationCoords - funkcja pomocnicza zwracajaca współrzędne stacji która pasuje do przynajmniej jednego z podanych parametrów

matchingStationCoords(Name, Coords, Stations) ->
  MatchingStations = lists:filter(fun ({N, C}) -> (N == Name) or (C == Coords) end, Stations),
  case MatchingStations of
    [{_, C}] -> C;
    _ -> none
  end.

%% addStation - działanie funkcji zgodne z instrukcją do labolatoriów

addStation(Name, Coords, Monitor) ->
  Stations = Monitor#monitor.stations,
  ExistingCoords = matchingStationCoords(Name, Coords, Stations),
  case is_tuple(ExistingCoords) of
    false -> Monitor#monitor{stations=[{Name, Coords}] ++ Stations};
    _ -> {error, station_exist}
  end.

%% addValue - działanie funkcji zgodne z instrukcją do labolatoriów

addValue(StationInfo, Date, Type, Value, Monitor) ->
  Stations = Monitor#monitor.stations,
  Coords = matchingStationCoords(StationInfo, StationInfo, Stations),
  case is_tuple(Coords) of
    true ->
      Values = Monitor#monitor.values,
      case maps:is_key({Coords, Date, Type}, Values) of
        false -> Monitor#monitor{values=maps:put({Coords, Date, Type}, Value, Values)};
        _ -> {error, value_exist}
      end;
    _ -> {error, station_dont_exist}
  end.

%% removeValue - działanie funkcji zgodne z instrukcją do labolatoriów

removeValue(StationInfo, Date, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  Coords = matchingStationCoords(StationInfo, StationInfo, Stations),
  Monitor#monitor{values=maps:remove({Coords, Date, Type}, Monitor#monitor.values)}.

%% getOneValue - działanie funkcji zgodne z instrukcją do labolatoriów

getOneValue(StationInfo, Date, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  Coords = matchingStationCoords(StationInfo, StationInfo, Stations),
  case is_tuple(Coords) of
    true ->
      Key = {Coords, Date, Type},
      Map = Monitor#monitor.values,
      case maps:is_key(Key, Map) of
        true -> maps:get(Key, Map);
        _ -> {error, value_dont_exist}
      end;
    _ -> {error, station_dont_exist}
  end.

%% getStationMean - działanie funkcji zgodne z instrukcją do labolatoriów

getStationMean(StationInfo, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  Coords = matchingStationCoords(StationInfo, StationInfo, Stations),
  case is_tuple(Coords) of
    true ->
      Values = maps:to_list(Monitor#monitor.values),
      FilteredValues =
        lists:filter(
          fun ({ {C, _, T}, _ }) ->
            if
              (Coords == C) and (Type == T) -> true;
              true -> false
            end
          end, Values),
      case length(FilteredValues) of
        0 -> {error, no_matching_values};
        _ -> lists:foldl(fun ({ _, Val }, Sum) -> Sum + Val end, 0, FilteredValues) / length(FilteredValues)
      end;
    _ -> {error, station_dont_exist}
  end.

%% getDailyMean - działanie funkcji zgodne z instrukcją do labolatoriów

getDailyMean(Date, Type, Monitor) ->
  Values = maps:to_list(Monitor#monitor.values),
  FilteredValues =
    lists:filter(
      fun ({ {_, D, T}, _ }) ->
        {Day, _} = D,
        if
          (Day == Date) and (Type == T) -> true;
          true -> false
        end
      end, Values),
  case length(FilteredValues) of
    0 -> {error, no_matching_values};
    _ -> lists:foldl(fun ({ _, Val }, Sum) -> Sum + Val end, 0, FilteredValues) / length(FilteredValues)
  end.

%% funkcja pomocnicza do getDailyMaxValue

maxValue({{Coords, _, _}, Val}, []) -> {Coords, Val};
maxValue({_, MaxValue}, [{Coords, Value} | T]) when MaxValue < Value -> maxValue({Coords, Value}, T);
maxValue(Max, [_ | T]) -> maxValue(Max, T).

%% getDailyMaxValue - zwraca pomiar o największej wartości w danym dniu i o danym typie

getDailyMaxValue(Date, Type, Monitor) ->
  Values = maps:to_list(Monitor#monitor.values),
  case lists:filter(fun ({{ _, {D, _}, T }, _}) -> (D == Date) and (T == Type) end, Values) of
    [] -> {error, no_matching_values};
    [H | T] -> maxValue(H, T)
  end.

%% getDailyValueCount - zwraca ilość pomiarów danego typu danego dnia

getDailyValueCount(Date, Type, Monitor) ->
  Values = maps:filter(fun ({_, {D, _}, T}, _) -> (D == Date) and (T == Type) end, Monitor#monitor.values),
  maps:size(Values).