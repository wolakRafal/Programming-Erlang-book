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
-export([start_link/0, is_prime/1]).

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
start_link() ->
  io:format("start_link ~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns true if P is prime, false otherwise
is_prime(P) ->
  gen_server:call(?SERVER, {is_prime, P}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  io:format("prime_tester_server worker starting~n"),
  {ok, 0}.

handle_call({is_prime, P}, _From, N) ->
  {reply, lib_primes:is_prime(P), N + 1};

handle_call(_Request, _From, State) ->  {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) ->  {noreply, State}.
terminate(_Reason, _State) ->  ok.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
