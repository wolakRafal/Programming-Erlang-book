%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo
%%% @doc YAFS - Yet Another File Server - my own implementation based on lib_chan
%%%
%%% @end
%%% Created : 26. cze 2016 19:08
%%%-------------------------------------------------------------------
-module(yafs).
-author("Rafal Wolak").

%% API
-export([start_server/3]).


%% MM is the PID of a proxy process that is used to talk to the client.
start_server(MM, _ArgsC, _ArgS) ->
  loop(MM).

loop(MM) ->
  receive
    {chan, MM, {put, Name, Bin}} ->
      io:format("Store file ~p ", [Name]),
      file:write_file(Name, Bin),
      MM ! ok,
      loop(MM);
    {chan, MM, {dir, Path}} ->
      io:format("Dir of path ~p ", [Path]),
      MM ! file:list_dir(Path),
      loop(MM);
    {chan, MM, {get, Name}} ->
      io:format("Returning file ~p ", [Name]),
      MM ! file:read_file(Name),
      loop(MM);
    {chan_closed, MM} ->
      true
  end.
