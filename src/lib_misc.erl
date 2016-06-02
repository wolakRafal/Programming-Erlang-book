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
    io:format("Process ~p Died. Lived for ~p. Reason ~p", [Pid, T-StartTime, Why]) end),
  Pid.

my_spawn(Mod, Func, Args) ->
  {StartTime, _} = statistics(wall_clock),
  Pid = spawn(Mod, Func, Args),
  on_exit(Pid, fun(Why) ->
    {T, _} = statistics(wall_clock),
    io:format("Process ~p Died. Lived for ~p seconds. Reason ~p~n", [Pid, (T-StartTime)/1000, Why]) end),
  Pid.
