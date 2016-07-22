-module(queue_server).
-author("RafalW").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,  handle_call/3,  handle_cast/2,  handle_info/2,  terminate/2,  code_change/3]).

-define(SERVER, ?MODULE).
-define(WORKERS_SUP, prime_tester_server_supervisor).

-record(state, {task_queue  = [],
                worker_pool = []}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  N = 10,
  io:format("queue_server starting~n"),
  io:format("queue_server create ~p workers~n", [N]),
  {ok, Pid} = supervisor:start_link({local, ?WORKERS_SUP}, prime_tester_server_supervisor, _Arg = [{noWorkers, N}]),
  {ok, #state{}}.

handle_call({is_prime, P}, _From, State) ->
  {reply, lib_primes:is_prime(P), State};

handle_call(Other, _From, State) ->
  error_logger:error_msg("*** queue_server receive unknow message ~p ~n", [Other]),
  {reply, bad_call, State}.

handle_cast(_Request, State) ->  {noreply, State}.

handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, _State) ->  ok.

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
