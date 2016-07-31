-module(queue_server).
-author("RafalW").

-behaviour(gen_server).

%% API
-export([start_link/0, get_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(WORKERS_SUP, prime_tester_server_supervisor).

-record(state, {tasks = [],
  active_workers = []}).

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
  io:format("queue_server starting~n"),
  io:format("queue_server create ~p workers~n", [N]),
  {ok, _Pid} = supervisor:start_link({local, ?WORKERS_SUP}, prime_tester_server_supervisor, _Arg = [{noWorkers, N}]),
  {ok, #state{}}.

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(Other, _From, State) ->
  error_logger:error_msg("*** queue_server receive unknow message ~p ~n", [Other]),
  {reply, bad_call, State}.

handle_cast({is_prime, P, From}, S) ->
  NewState = case S#state.active_workers of
               [Worker | Rest] ->
                 io:format("Client ~p needs check prime ~p. Sending job is_prime to ~p~n", [From, P, Worker]),
                 Worker ! {job, fun lib_primes:is_prime/1, P, From},
                 S#state{active_workers = Rest};
               [] ->
                 io:format("No available workers right now, need to enqueue this job~n"),
                 S#state{tasks = [{job, fun lib_primes:is_prime/1, P, From} | S#state.tasks]}
             end,
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info({work_wanted, From}, S) ->
  io:format("Worker ~p need work.~n", [From]),
  case S#state.tasks of
    [] ->
      io:format("No Work have to wait.~n"),
      {noreply, S#state{active_workers = [From | S#state.active_workers]}};
    [Job | Tail] ->
      io:format("Got some work for you: ~p.~n", [Job]),
      gen_server:cast(?SERVER, Job),
      {noreply, S#state{tasks = Tail}}
  end;


%% TODO: handle EXIT signals from workers
handle_info(Other, State) ->
  error_logger:error_msg("*** queue_server receive unknow message ~p ~n", [Other]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
