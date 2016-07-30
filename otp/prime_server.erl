%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2016 18:29
%%%-------------------------------------------------------------------
-module(prime_server).
-author("Rafal Wolak").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_prime(N) ->
  %% 20000 is a timeout (ms)
  gen_server:call(?SERVER, {prime, N}, 20000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  %% Note we must set trap_exit = true if we
  %% want terminate/2 to be called when the application
  %% is stopped
  process_flag(trap_exit, true),
  io:format("~p starting ~n",  [?SERVER]),
  {ok, 0}.

handle_call({prime, K}, _From, N) ->
  {reply, make_new_prime(K), N+1};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p stopping~n",[?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_new_prime(K) ->
  if
    K > 100 ->
      alarm_handler:set_alarm(tooHot),
      N = lib_primes:make_prime(K),
      alarm_handler:clear_alarm(tooHot),
      N;
    true ->
      lib_primes:make_prime(K)
  end.
