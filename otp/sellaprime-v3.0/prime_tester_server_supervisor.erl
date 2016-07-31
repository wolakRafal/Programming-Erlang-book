-module(prime_tester_server_supervisor).
-author("RafalW").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  start_link(10).

start_link(N) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [{noWorkers, N}]).

%%%===================================================================
%%% Supervisor callbacks
%%%==================================================================
init([{noWorkers, N}]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 1000,
  Type = worker,
  Workers = [{list_to_atom("prime_tester_worker" ++ integer_to_list(X)) , {prime_tester_server, start_link, [integer_to_list(X)]},
              Restart, Shutdown, Type, [prime_tester_server]} || X <- lists:seq(1, N)],
  {ok, {SupFlags, Workers}}.
