%%%-------------------------------------------------------------------
%%% @author Rafal Wolak
%%% @copyright (C) 2016, robo software innovations
%%% @doc
%%%       Job management service.
%%%  The job center keeps a queue of jobs that have to be done.
%%%  The jobs are numbered.
%%%  Anybody can add jobs to the queue.
%%% Workers can request jobs from the queue and tell the job center
%%% that a job has been performed. Jobs are represented by funs.
%%% To do a job F, a worker must evaluate the function F().
%%%
%%% @end
%%% Created : 11. Jul 2016 20:02
%%%-------------------------------------------------------------------
-module(job_center).
-author("Rafal Wolak").

-behaviour(gen_server).

%% API
-export([start_link/0, add_job/1, work_wanted/0, job_done/1, test_server/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {jobs :: []}).

%%%===================================================================
%%% API
%%%===================================================================
%% Start the job center server
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_job(F) ->
  {ok, JobNumber} = gen_server:call(?SERVER, {add_job, F}),
  JobNumber.

%%  returns {JobNumber,F} | no.
work_wanted() ->
  gen_server:call(?SERVER, work_wanted).

job_done(JobNumber) ->
  gen_server:call(?SERVER, {job_done, JobNumber}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{jobs = []}}.

handle_call({add_job, F}, _From, State) ->
  JobNumber = erlang:unique_integer([positive, monotonic]),
  NewState = State#state{jobs = [{JobNumber, F, 0} | State#state.jobs]},
  {reply, {ok, JobNumber}, NewState};

handle_call(work_wanted, _From, State) ->
  JobsLeft = [ X || X={_,_,0} <- State#state.jobs],
  Resp = case lists:reverse(JobsLeft) of
           [] -> no;
           [{JobNumber, F, 0} | _] -> {JobNumber, F}
         end,
  NewJobs = lists:map(fun ({JobNumber, F , X}) ->
                          if {JobNumber, F} == Resp ->
                            {JobNumber, F, 1};
                          true ->
                            {JobNumber, F , X}
                          end
                      end, State#state.jobs),
  NewState = State#state{jobs = NewJobs},
  {reply, Resp, NewState};

handle_call({job_done, JobNumber}, _From, State) ->
  Completed = [X || X = {JobN, _, 1} <- State#state.jobs, JobN == JobNumber],
  {Response, Jobs} = case Completed of
                  [E] -> {ok, lists:delete(E, State#state.jobs)};
               [] -> {{error, bad_job_number}, State#state.jobs}
  end,
  {reply, Response, #state{jobs = Jobs}};

handle_call(Request, From, State) ->
  io:format("Server receives unexpeted request~p from ~p~n", [Request, From]),
  {reply, bad_call, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% TESTS
%%%===================================================================

test_server() ->
  job_center:start_link(),
  J1 = job_center:add_job(fun () -> io:format("Job number 1~n") end),
  J2 = job_center:add_job(fun () -> io:format("Job number 2~n") end),
  J3 = job_center:add_job(fun () -> io:format("Job number 3~n") end),
  io:format("Job numbers: ~p, ~p, ~p~n", [J1, J2, J3]),
  {J1, F1} = job_center:work_wanted(),
  F1(),
  {J2, F2} = job_center:work_wanted(),
  F2(),
  ok = job_center:job_done(J1),
  ok = job_center:job_done(J2),
  {error, _} = job_center:job_done(J2),
  {error, _} = job_center:job_done(J3),
  {J3, _} = job_center:work_wanted(),
  ok = job_center:job_done(J3),
  no = job_center:work_wanted(),
  io:format("SUCCESS~n"),
  done.




