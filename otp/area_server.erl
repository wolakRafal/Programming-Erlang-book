%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2016 18:39
%%%-------------------------------------------------------------------
-module(area_server).
-author("Rafal Wolak").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %% Note we must set trap_exit = true if we
  %% want terminate/2 to be called when the application %% is stopped
  process_flag(trap_exit, true),
  io:format("~p starting~n",[?MODULE]),
  {ok, 0}.

handle_call({area, Thing}, _From, N) -> {reply, compute_area(Thing), N+1}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _N) ->
  io:format("~p stopping~n",[?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
compute_area({square, X})       -> X*X;
compute_area({rectongle, X, Y}) -> X*Y. %% HERE IS DELIBERATE ERROR

