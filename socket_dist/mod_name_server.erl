%%%-------------------------------------------------------------------
%%% @author RafalW
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% socket_socket_dist
%%% @end
%%% Created : 06. Jun 2016 14:01
%%%-------------------------------------------------------------------
-module(mod_name_server).
-author("RafalW").

%% API
-export([start_me_up/3]).


%% MM is the PID of a proxy process that is used to talk to the client.
start_me_up(MM, _ArgsC, _ArgS) ->
  loop(MM).

loop(MM) ->
  receive
    {chan, MM, {store, K, V}} ->
      kvs:store(K, V),
      loop(MM);
    {chan, MM, {lookup, K}} ->
      MM ! {send, kvs:lookup(K)},
      loop(MM);
    {chan_closed, MM} ->
      true
  end.
