%%%-------------------------------------------------------------------
%%% @author Kamil Krzempek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2020 12:34
%%%-------------------------------------------------------------------
-module(lab3_pingpong).
-author("Kamil Krzempek").

%% API
-export([start/0, stop/0, play/1]).

start() ->
  Ping = fun Loop(Sum) ->
          receive
            {play, N} ->  self() ! {bounce, N}, Loop(Sum + N);
            {bounce, 0} -> Loop(Sum);
            {bounce, N} ->
              io:format("Ping bounce. Sum from play messages: ~B~n", [Sum]),
              timer:sleep(100),
              pong ! {bounce, N - 1},
              Loop(Sum);
            stop -> ok
          after
            20000 -> io:format("Process idle, exiting~n"), ok
          end
         end,
  Pong = fun Loop() ->
          receive
            {bounce, 0} -> Loop();
            {bounce, N} ->
              io:format("Pong bounce~n"),
              timer:sleep(100),
              ping ! {bounce, N - 1},
              Loop();
            stop -> ok
          after
            20000 -> io:format("Process idle, exiting~n"), ok
          end
         end,
  PingPID = spawn(fun() -> Ping(0) end),
  PongPID = spawn(Pong),
  register(ping, PingPID),
  register(pong, PongPID).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) -> ping ! {play, N}.