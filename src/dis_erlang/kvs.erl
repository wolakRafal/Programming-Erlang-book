%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%% Stage 1: A Simple Name Server
%%% Our name server kvs is a simple Key → Value, server. It has the following inter- face:
%%% -spec kvs:start() -> true
%%% Start the server; this creates a server with the registered name kvs. -spec kvs:store(Key, Value) -> true
%%% Associate Key with Value.
%%% -spec kvs:lookup(Key) -> {ok, Value} | undefined
%%% Look up the value of Key, and return {ok, Value} if there is a value associated with Key; otherwise, return undefined.
%%%
%%% @end
%%% Created : 05. Jun 2016 21:55
%%%-------------------------------------------------------------------
-module(kvs).
-author("Rafal Wolak").

%% API
-export([store/2, start/0, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->
  kvs ! {self(), Q},
  receive
    {kvs, Reply} ->
      Reply
  end.

loop() -> receive
            {From, {store, Key, Value}} ->
              put(Key, {ok, Value}),
              From ! {kvs, ok},
              loop();
            {From, {lookup, Key}} ->
              From ! {kvs, get(Key)},
              loop()
          end.