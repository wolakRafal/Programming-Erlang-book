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

-define(SERVER, queue_server). % now points to queue_server

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
      io:format("Server Busy. State of queue_server ~p ~n", [queue_server:get_state()]),
      server_busy
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  io:format("prime_tester_server worker starting~n"),
  ?SERVER ! {work_wanted , self()},
  {ok, 0}.

handle_call({job, F, Arg}, _From, N) ->
  {reply, F(Arg), N + 1};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, N) ->
  {noreply, N + 1}.

handle_info({job, F, Arg, Pid}, N) ->
  timer:sleep(300),
  Pid ! F(Arg),
  queue_server ! {work_wanted, self()},
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
%%%    2> prime_tester_server:tests().
tests() ->
  application:start(sellaprime),
  receive
  after 300 -> ok end,
  {state, [], ActiveWorkers} = queue_server:get_state(),
  10 = length(ActiveWorkers),
  [spawn(fun () -> prime_tester_server:is_prime(X)end) || X <- lists:seq(1,10)],
  prime_tester_server:is_prime(5555),
  [spawn(fun () -> prime_tester_server:is_prime(X)end) || X <- lists:seq(11,15)],
  prime_tester_server:is_prime(777777),
  pass.
