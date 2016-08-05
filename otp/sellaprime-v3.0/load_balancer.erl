%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%% A load balancer that keeps track of the work being done and requests to be done by the prime server testers.
%%% Requests to test new primes should now be sent to the load balancer.
%%% Arrange that the load balancer sends requests to the least loaded server.
%%% @end
%%% Created : 31. Jul 2016 15:39
%%%-------------------------------------------------------------------
-module(load_balancer).
-author("Rafal Wolak").

-behaviour(gen_server).

%% API
-export([start_link/0, get_state/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(WORKERS_SUP, prime_tester_server_supervisor).

-record(state, {workers = #{}}).

-record(worker, {pid,
  load}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% for unit tests
get_state() ->
  gen_server:call(?SERVER, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  N = 10,
  io:format("load_balancer server starting~n"),
  io:format("load_balancer create ~p workers~n", [N]),
  {ok, _Pid} = supervisor:start_link({local, ?WORKERS_SUP}, prime_tester_server_supervisor, _Arg = [{noWorkers, N}]),
  {ok, #state{}}.

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(Job = {is_prime, _P, _From}, S) ->
  W = getLeastLoadedWorker(S),
%%  io:format("From list ~p I've picked ~p ~n ", [S#state.workers, W]),
  Pid = W#worker.pid,
  Pid ! Job,
  NewWorker = W#worker{load = W#worker.load + 1},
  NewState = S#state{workers = maps:update(Pid, NewWorker, S#state.workers)},
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info({im_ready, Pid}, S) ->
  io:format("register worker ~p~n", [Pid]),
  monitor(process, Pid),
  {noreply, S#state{workers = maps:put(Pid, #worker{pid = Pid, load = 0}, S#state.workers)}};

handle_info({job_done, Pid}, S) ->
%%  io:format("Worker ~p done his job ~n", [Pid]),
  {noreply, S#state{workers = reduceLoad(Pid, S#state.workers)}};


%%  handle EXIT signals from workers
handle_info({'DOWN', _Ref, process, Pid, Reason}, S) ->
  error_logger:error_msg("Worker ~p died. Reason:~p. Remove from registry ~n", [Pid, Reason]),
  NewState = S#state{workers = maps:remove(Pid, S#state.workers)},
  {noreply, NewState};

handle_info(Other, S) ->
  error_logger:error_msg("Received unknow message~p~n", [Other]),
  {noreply, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% returns least loaded worker
getLeastLoadedWorker(S) when is_record(S, state) ->
  Map = S#state.workers,
  maps:fold(fun(_K, V={worker, _, Load}, Acc={worker, _ , MaxLoad}) ->
              if
                Load =< MaxLoad ->
                  V;
                true ->
                  Acc
              end
            end, hd(maps:values(Map)), Map).

reduceLoad(Pid, Workers) when is_map(Workers) ->
  W = maps:get(Pid, Workers),
  NewWorker = W#worker{load = W#worker.load - 1 },
  maps:update(Pid, NewWorker, Workers).