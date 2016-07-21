%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2016 15:01
%%%-------------------------------------------------------------------
-module(my_alarm_handler).
-author("Rafal Wolak").

-behaviour(gen_event).

%% API
-export([start_link/0,
  add_handler/0]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================
init(Args) ->
  io:format("*** my_alarm_handler init:~p~n",[Args]),
  {ok, 0}.

handle_event({set_alarm, tooHot}, N) ->
  error_logger:error_msg("*** Tell the Engineer to turn on the fan~n"),
  {ok, N+1};

handle_event({clear_alarm, tooHot}, N) ->
  error_logger:error_msg("*** Danger over. Turn off the fan~n"),
  {ok, N};

handle_event(Event, N) ->
  io:format("*** unmatched event:~p~n",[Event]),
  {ok, N}.


handle_call(_Request, State) -> Reply = ok, {ok, Reply, State}.
handle_info(_Info, State) -> {ok, State}.

terminate(_Arg, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
