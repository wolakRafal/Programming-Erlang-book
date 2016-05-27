-module(a).
-compile(export_all).

start(Tag) ->
  spawn(fun() -> loop(Tag) end).

loop(Tag) ->
  sleep(),
  Val = b:x(),
  io:format("Vsn1 (~p) b:x() = ~p~n",[Tag, Val]),
  a:loop(Tag).

sleep() ->
  receive
    after 3000 -> true
  end.