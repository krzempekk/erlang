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
  {reply, pollution:getDailyValueCount(Date, Type, State), State}.


handle_cast(crash, State) ->
  exit("Error");

handle_cast({addStation, Name, Coords}, State) ->
  {noreply, pollution:addStation(Name, Coords, State)};

handle_cast({addValue, StationInfo, Date, Type, Value}, State) ->
  {noreply, pollution:addValue(StationInfo, Date, Type, Value, State)};

handle_cast({removeValue, StationInfo, Date, Type}, State) ->
  {noreply, pollution:removeValue(StationInfo, Date, Type, State)}.


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
  gen_server:cast(?MODULE, {addStation, Name, Coords}).

addValue(StationInfo, Date, Type, Value) ->
  gen_server:cast(?MODULE, {addValue, StationInfo, Date, Type, Value}).

removeValue(StationInfo, Date, Type) ->
  gen_server:cast(?MODULE, {removeValue, StationInfo, Date, Type}).

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