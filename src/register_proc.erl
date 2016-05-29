%%%-------------------------------------------------------------------
%% Exercises
% 1. Write a function start(AnAtom, Fun) to register AnAtom as spawn(Fun).
% Make sure your program works correctly in the case when two parallel processes simultaneously evaluate start/2.
% In this case, you must guarantee that one of these processes succeeds and the other fails.
%%%-------------------------------------------------------------------
-module(register_proc).

%% API
-export([start/2]).


start(AnAtom, Fun) ->
  P = whereis(AnAtom),
  if
    is_pid(P) ->
      error(atom_to_list(AnAtom) ++ " - Name already taken");
    true ->
      Pid = spawn(Fun),
      register(AnAtom, Pid),
      Pid
  end.

