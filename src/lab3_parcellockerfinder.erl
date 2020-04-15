%%%-------------------------------------------------------------------
%%% @author Kamil Krzempek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 13:07
%%%-------------------------------------------------------------------
-module(lab3_parcellockerfinder).
-author("Kamil Krzempek").

%% API
-export([generateData/2, distance/2, closestLocker/3, findMyParcelLocker/2, computeSequential/2, measureTime/2, computeParallel/2, computeMixed/3]).

generateData(PCount, LCount) ->
  MaxX = 1000, MaxY = 1000,
  PersonLocations = [{random:uniform() * MaxX, random:uniform() * MaxY} || _ <-lists:seq(1, PCount)],
  LockerLocations = [{random:uniform() * MaxX, random:uniform() * MaxY} || _ <-lists:seq(1, LCount)],
  { PersonLocations, LockerLocations }.

distance({ X1, Y1 }, { X2, Y2 }) -> math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

closestLocker(_, BestLLoc, []) -> BestLLoc;
closestLocker(PLoc, BestLLoc, [LLoc | T]) ->
  ShorterDist = distance(PLoc, BestLLoc) > distance(PLoc, LLoc),
  case ShorterDist of
      true -> closestLocker(PLoc, LLoc, T);
      false -> closestLocker(PLoc, BestLLoc, T)
  end.

findMyParcelLocker(PLoc, [LLoc | T]) -> closestLocker(PLoc, LLoc, T).

computeSequential(PLocs, LLocs) -> [ findMyParcelLocker(PLoc, LLocs) || PLoc <- PLocs ].


computeParallel(PLocs, LLocs) ->
  Pid = self(),
  Compute = fun(PLoc) ->
              Pid ! findMyParcelLocker(PLoc, LLocs)
            end,
  Receive = fun() ->
              receive
                {X, Y} -> {X, Y}
              end
            end,

  [spawn(fun() -> Compute(PLoc) end) || PLoc <- PLocs],
  [Receive() || _ <- PLocs].


computeMixed(PLocs, LLocs, ProcCount) ->
  Pid = self(),
  Compute = fun(PLocations) ->
              [ Pid ! findMyParcelLocker(PLoc, LLocs) || PLoc <- PLocations ]
            end,
  Receive = fun() ->
              receive
                {X, Y} -> {X, Y}
              end
            end,
  PartMaxLen = length(PLocs) div ProcCount,
  StartIndices = lists:seq(1, length(PLocs), PartMaxLen),
  [spawn(fun() -> Compute(lists:sublist(PLocs, Index, PartMaxLen)) end) || Index <- StartIndices],
  [Receive() || _ <- PLocs].


measureTime(PCount, LCount) ->
  { PLocs, LLocs } = generateData(PCount, LCount),
  { Time1, _ } = timer:tc(?MODULE, computeSequential, [ PLocs, LLocs ]),
  io:format("computeSequential time: ~wms~n", [Time1 / 1000]),
  { Time2, _ } = timer:tc(?MODULE, computeParallel, [ PLocs, LLocs ]),
  io:format("computeParallel time: ~wms~n", [Time2 / 1000]),
  { Time3, _ } = timer:tc(?MODULE, computeMixed, [ PLocs, LLocs, 4 ]),
  io:format("computeMixed time: ~wms~n", [Time3 / 1000]).

