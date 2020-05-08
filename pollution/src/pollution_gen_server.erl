%%%-------------------------------------------------------------------
%%% @author Kamil Krzempek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start/0, stop/0, crash/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDailyMaxValue/2, getDailyValueCount/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) -> {ok, pollution:createMonitor()}.


handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call({getOneValue, StationInfo, Date, Type}, _From, State) ->
  {reply, pollution:getOneValue(StationInfo, Date, Type, State), State};

handle_call({getStationMean, StationInfo, Type}, _From, State) ->
  {reply, pollution:getStationMean(StationInfo, Type, State), State};

handle_call({getDailyMean, Date, Type}, _From, State) ->
  {reply, pollution:getDailyMean(Date, Type, State), State};

handle_call({getDailyMaxValue, Date, Type}, _From, State) ->
  {reply, pollution:getDailyMaxValue(Date, Type, State), State};

handle_call({getDailyValueCount, Date, Type}, _From, State) ->
  {reply, pollution:getDailyValueCount(Date, Type, State), State};

% W przypadku błędu związanego ze stanem bazy (np. taka stacja już istnieje), zachowujemy poprzedni stan i zwracamy userowi błąd. Funkcje modyfikujące bazę zostały zrealizowane jako calle, aby klient dostał informację o powodzeniu (wtedy atom ok) lub błędzie (wtedy krotka błędu która została zwrócona z funkcji modułu pollution)

handle_call({addStation, Name, Coords}, _From, State) ->
  NewState = pollution:addStation(Name, Coords, State),
  case NewState of
    {error, _} -> {reply, NewState, State};
    _ -> {reply, ok, NewState}
  end;

handle_call({addValue, StationInfo, Date, Type, Value}, _From, State) ->
  NewState = pollution:addValue(StationInfo, Date, Type, Value, State),
  case NewState of
    {error, _} -> {reply, NewState, State};
    _ -> {reply, ok, NewState}
  end;

handle_call({removeValue, StationInfo, Date, Type}, _From, State) ->
  NewState = pollution:removeValue(StationInfo, Date, Type, State),
  case NewState of
    {error, _} -> {reply, NewState, State};
    _ -> {reply, ok, NewState}
  end.


handle_cast(crash, _State) ->
  exit("Error").


handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Interface
%%%===================================================================

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, terminate).

crash() -> gen_server:cast(?MODULE, crash).

addStation(Name, Coords) ->
  gen_server:call(?MODULE, {addStation, Name, Coords}).

addValue(StationInfo, Date, Type, Value) ->
  gen_server:call(?MODULE, {addValue, StationInfo, Date, Type, Value}).

removeValue(StationInfo, Date, Type) ->
  gen_server:call(?MODULE, {removeValue, StationInfo, Date, Type}).

getOneValue(StationInfo, Date, Type) ->
  gen_server:call(?MODULE, {getOneValue, StationInfo, Date, Type}).

getStationMean(StationInfo, Type) ->
  gen_server:call(?MODULE, {getStationMean, StationInfo, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(?MODULE, {getDailyMean, Date, Type}).

getDailyMaxValue(Date, Type) ->
  gen_server:call(?MODULE, {getDailyMaxValue, Date, Type}).

getDailyValueCount(Date, Type) ->
  gen_server:call(?MODULE, {getDailyValueCount, Date, Type}).