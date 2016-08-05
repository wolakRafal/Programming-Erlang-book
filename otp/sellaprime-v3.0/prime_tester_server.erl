%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%   tests if a given number is prime
%%% @end
%%% Created : 21. Jul 2016 14:21
%%%-------------------------------------------------------------------
-module(prime_tester_server).
-author("Rafal Wolak").

-behaviour(gen_server).

%% API
-export([start_link/1, is_prime/1, tests/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, load_balancer). % now points to load_balancer
-define(MAGIC_NUMBER, 28376591248).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name) ->
  io:format("start_link ~n"),
  WorkerName = list_to_atom("Worker_" ++ Name),
  gen_server:start_link({local, WorkerName}, ?MODULE, [], []).

%% returns true if P is prime, false otherwise
is_prime(P) ->
  gen_server:cast(?SERVER, {is_prime, P, self()}),
  receive
    Resp ->
      Resp
    after 1000*60 -> %% one minute
      io:format("Server Busy. State of load_balancer  ~p ~n", [load_balancer:get_state()]),
      server_busy
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  io:format("prime_tester_server worker starting~n"),
  ?SERVER ! {im_ready , self()},
  {ok, 0}.

handle_call(_Request, _From, N) ->
  {reply, ok, N + 1}.

handle_cast(_Request, N) ->
  {noreply, N + 1}.

%% Blows away process - for tests
handle_info({is_prime, ?MAGIC_NUMBER, _From}, _N) ->
  exit(buuum);

handle_info({is_prime, P, From}, N) ->
  timer:sleep(300), %% delay for tests
  From ! (catch lib_primes:is_prime(P)),
  load_balancer ! {job_done, self()},
  {noreply, N + 1};

handle_info(_Info, State) ->  {noreply, State}.
terminate(_Reason, _State) ->  ok.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Tests
%%%===================================================================
%% prime_tester_server:tests().
tests() ->
  io:format("SELLAPRIME v3.0 TESTS STARTING~n"),
  application:start(sellaprime),
  receive
  after 300 -> ok end,
  {state, Workers} = load_balancer:get_state(),
%%  io:format("State:~p~n", [State]),
  10 = maps:size(Workers),
  [spawn( fun () -> prime_tester_server:is_prime(X) end) || X <- lists:seq(1,10)],
  timer:sleep(100),
  {state, S1} = load_balancer:get_state(),
%%  io:format("State:~p~n", [S1]),
  10 = length([X || {X, {worker, X, 1}} <- maps:to_list(S1)]),

  [spawn( fun () -> prime_tester_server:is_prime(X) end) || X <- lists:seq(1,5)],
  timer:sleep(50),
  {state, S2} = load_balancer:get_state(),
%%  io:format("State2:~p~n", [S2]),
  5 = length([X || {X, {worker, X, 2}} <- maps:to_list(S2)]),
  timer:sleep(400), % all workers should complete work
  pass = crash_test(),
  io:format("SELLAPRIME v3.0 TESTS FINISHED SUCCESSFULLY~n"),
  pass.


crash_test() ->
  io:format("Crash tests starting~n"),
  [spawn( fun () -> prime_tester_server:is_prime(X) end) || X <- lists:seq(1,40)],
  timer:sleep(100),
  {state, S1} = load_balancer:get_state(),
  io:format("State befor crash:~p~n", [S1]),
  10 = length([X || {X, {worker, X, 4}} <- maps:to_list(S1)]),
  spawn(fun () -> prime_tester_server:is_prime(?MAGIC_NUMBER)end), %% This will blow one worker
  timer:sleep(100),
  {state, S2} = load_balancer:get_state(),
  9 = length([X || {X, {worker, X, 4}} <- maps:to_list(S2)]),
%%  io:format("State:~p~n", [S1]),
  pass.
