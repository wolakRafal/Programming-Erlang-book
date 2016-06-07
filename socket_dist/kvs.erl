-module(kvs).
-author("RafalW").

%% API
-export([start/0, store/2, lookup/1, stop/0]).

start() ->
  Pid = spawn(fun loop/0),
  register(name_server, Pid),
  ok.


store(K, V) ->
  name_server ! {self(), {store, K, V}},
  receive
    {kvs, ok} ->
      ok
  end.

lookup(Key) ->
  name_server ! {self(), {lookup, Key}},
  receive
    {kvs, Response} ->
      Response
  end.

stop() ->
  name_server ! die.

loop() ->
  receive
    {From, {store, K, V}} ->
      put(K, V),
      From ! {kvs, ok},
      loop();
    {From, {lookup, Key}} ->
      From ! {kvs, get(Key)},
      loop();
    die ->
      ok
  end.