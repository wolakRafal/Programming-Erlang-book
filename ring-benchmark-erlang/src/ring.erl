%%%-------------------------------------------------------------------
%% A ring benchmark.
%% Create N processes in a ring.
%% Send a message round the ring M times so that a total of N * M messages get sent.
%% Measure Time how long this takes for different values of N and M.
%% %%%-------------------------------------------------------------------
-module(ring).
-author("Rafal Wolak").

%% API
-export([benchmark/2]).


benchmark(N, M) ->
  io:format("Ring Benchmark. ~p processes, sent ~p times ~n", [N, M]),
  % create a ring
  Ring = spawn(fun proc/0),
  Ring ! {create_ring, self(), N},

  receive
    {Last, ring_created} ->
      io:format("Sending message~n"),
      Ring ! {message, Ring, Last, M};
    Any ->
      io:format("Wrong message received~p~n", [Any])
  end,

  statistics(runtime),
  statistics(wall_clock),

  receive
    done -> ok
  after
      60000 -> io:format("Timed Out~n")
  end,
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
%%  lists:foreach(fun(Pid) -> Pid ! die end, L),
%%  U1 = Time1 * 1000,
%%  U2 = Time2 * 1000,

  io:format("Done in ~p (~p) miliseconds. Ring with ~p actors, message sent ~p times~n", [Time1, Time2, N, M]).

proc() ->
  receive
    {create_ring, Root, 0} ->
      io:format("Ring Created!~n"),
      Root ! {self(), ring_created},
      proc(Root);
    {create_ring, Root, N} ->
      Pid = spawn(fun proc/0),
      Pid ! {create_ring, Root, N - 1},
      proc(Pid)
  end.

proc(Next) ->
  receive
%%    die -> void;
    {message, _Begin, End, 0} when End == self() ->
      Next ! done, % send to the root
      exit(ok);
    {message, Begin, End, 0} ->
      Next ! {message, Begin, End, 0},
      exit(ok);
    {message, Begin, End, M} when End == self() ->
%%      io:format("Message passed all ring ~p~n", [M]),
      Begin ! {message, Begin, End, M - 1},
      proc(Next);
    {message, Begin, End, M} ->
      Next ! {message, Begin, End, M},
      proc(Next)
  end.
