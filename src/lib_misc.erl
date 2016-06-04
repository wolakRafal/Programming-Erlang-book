-module(lib_misc).
-author("Rafal Wolak").

-compile(export_all).

%% Write a function my_spawn(Mod, Func, Args)
%% that behaves like spawn(Mod, Func, Args)
%% but with one difference.
%% If the spawned process dies,
%% a message should be printed saying why the process
%% died and how long the process lived for before it died.
on_exit(Pid, Fun) ->
  spawn(fun() ->
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Why} ->
        Fun(Why)
    end
        end).

my_spawn(Func) ->
  {StartTime, _} = statistics(wall_clock),
  Pid = spawn(Func),
  on_exit(Pid, fun(Why) ->
    {T, _} = statistics(wall_clock),
    io:format("Process ~p Died. Lived for ~p. Reason ~p", [Pid, T - StartTime, Why]) end),
  Pid.

my_spawn(Mod, Func, Args) ->
  {StartTime, _} = statistics(wall_clock),
  Pid = spawn(Mod, Func, Args),
  on_exit(Pid, fun(Why) ->
    {T, _} = statistics(wall_clock),
    io:format("Process ~p Died. Lived for ~p seconds. Reason ~p~n", [Pid, (T - StartTime) / 1000, Why]) end),
  Pid.


%% Write a function my_spawn(Mod, Func, Args, Time) that behaves like spawn(Mod, Func, Args)
%% but with one difference.
%% If the spawned process lives for more than Time seconds, it should be killed.
my_spawn(Mod, Func, Args, Time) ->
  F = fun() ->
    Pid = spawn_link(Mod, Func, Args),
    receive
      X ->
        io:format("Forwarding message ~p to ~p ~n", [X, Pid]),
        Pid ! X
    after Time ->
      io:format("Timeout ~p milis. Exiting two processes ~n", [Time]),
      exit(timeout)
    end
      end,

  spawn(F).

echo() ->
  receive
    X ->
      io:format("I received message ~p ~n", [X]),
      echo()
  end.

f() ->
  receive
    X ->
      V = atom_to_list(X),
      io:format("Atom to list: ~p ~n", [V]),
      f()
  end.


%% Write a function that creates a registered process that writes out
%% "I'm still running" every five seconds.
%% Write a function that monitors this process and restarts it if it dies.
%% Start the global process and the monitor process.
%% Kill the global process and check that it has been restarted by the monitor process.
still_dre() ->
  io:format("I'm still running~n"),
  timer:sleep(5000),
  still_dre().

%% global process
running_all_day() ->
  spawn(lib_misc, still_dre, []).

%% monitor process
restarter(Pid) when is_pid(Pid) ->
  spawn(fun() ->
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, _Why} ->
        io:format("Process has died. Restarting"),
        restarter(running_all_day())
    end
        end);

restarter({M, F, A} = MFA) ->
  Pid = spawn(M, F, A),
  spawn(fun() ->
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Why} ->
        io:format("Process has died because: ~p. Restarting~n", [Why]),
        restarter(MFA)
    end
        end),
  Pid.

%% Write a function that starts and monitors several worker processes.
%% If any of the worker processes dies abnormally, restart it.
start_workers_monitor(MFA, _N) ->
  lists:map(fun(_) -> restarter(MFA) end, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).


%% Write a function that starts and monitors several worker processes.
%% If any of the worker processes dies abnormally,
%% kill all the worker processes and restart them all.
start_workers_all_for_one(M, F, A, N) ->
  Group = spawn(fun() ->
                  [spawn_link(M, F, A) || _ <- lists:seq(1, N)],
                    receive
                      die -> ok
                    end
                end),
  Ref = monitor(process, Group),
  receive
    {'DOWN', Ref, process, Group, Why} ->
      io:format("One of the workers die [~p]. Restart them all~n", [Why]),
      start_workers_all_for_one(M, F, A, N)
  end.