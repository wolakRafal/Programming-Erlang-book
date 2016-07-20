-module(sellaprime_supervisor).
-behaviour(supervisor).

%% API
-export([start/0, start_in_shell_for_testing/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start() ->
  spawn(fun() -> supervisor:start_link({local, ?SERVER}, ?MODULE, _Arg = []) end).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, _Arg = []),
  unlink(Pid).

start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
  %% Install my personal error handler
  gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {my_alarm_handler, xyz}),

  RestartStrategy = one_for_one,
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 10000,
  Type = worker,

  AreaServer = {tag1, {area_server, start_link, []},
    Restart, Shutdown, Type, [area_server]},

  PrimeServer = {tag2, {prime_server, start_link, []},
    Restart, Shutdown, Type, [prime_server]},

  {ok, {SupFlags, [AreaServer, PrimeServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
