-module(not_yet_implemented_macro).
-author("Rafal Wolak").

-compile(export_all).

-define(NYI(X),(begin
                  io:format("*** NYI ~p ~p ~p~n",[?MODULE, ?LINE, X]),
                  exit(nyi) end)).
