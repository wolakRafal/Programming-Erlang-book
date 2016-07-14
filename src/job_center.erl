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
-export([start_link/0, add_job/1, work_wanted/0, job_done/1, test_server/0, statistics/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(job, { id       :: integer(),  %% JobNumber
                f       :: fun() ,
                status  :: free | in_progress | done,
                worker  :: reference()}).

-record(state, {jobs        :: [],
                all_jobs    :: integer(),
                in_progress :: integer(),
                done        :: integer(),
                workers     :: any()}).

%%%===================================================================
%%% API
%%%===================================================================
%% Start the job center server
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_job(F) ->
  {ok, JobNumber} = gen_server:call(?SERVER, {add_job, F}),
  JobNumber.

%%  returns {JobNumber, F} | no.
work_wanted() ->
  J = gen_server:call(?SERVER, work_wanted),
  case J of
    no -> no;
    Job -> {Job#job.id, Job#job.f}
  end.

job_done(JobNumber) ->
  gen_server:call(?SERVER, {job_done, JobNumber}).

%% reports the status of the jobs in the queue and of jobs
%% that are in progress and that have been done.
%% returns {{all_jobs, N} , {in_progress, N}, (done, N}} where N is positive number
statistics() ->
  gen_server:call(?SERVER, stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{jobs = [],
              all_jobs = 0, in_progress = 0, done = 0,
              workers = dict:new() %% ref -> JobNumber
  }}.

handle_call({add_job, F}, _From, State) ->
  JobNumber = erlang:unique_integer([positive, monotonic]),
  NewState = State#state{jobs = [#job{id = JobNumber, f = F, status = free} | State#state.jobs], all_jobs = State#state.all_jobs + 1},
  {reply, {ok, JobNumber}, NewState};

handle_call(work_wanted, From, S) ->
  Job = get_free_job(S#state.jobs),
  NewJobs = mark_job_in_progress(Job, S#state.jobs),
  NewInProgress = S#state.in_progress + (if Job == no -> 0; true -> 1 end),
  NewState = S#state{jobs = NewJobs, in_progress = NewInProgress, workers = dict:append(From, Job, S#state.workers)},
  {reply, Job, NewState};

handle_call({job_done, JobNumber}, From, S) ->
  Completed = [X || X <- S#state.jobs, X#job.id == JobNumber, X#job.status == in_progress],
  {Response, NewJobs} = case Completed of
                        [E] -> {ok, lists:delete(E, S#state.jobs)};
                        [] -> {{error, bad_job_number}, S#state.jobs}
                    end,
  Done = (if Response == {error, bad_job_number} -> 0; true -> 1 end),
  NewDone = S#state.done + Done,
  NeInProgress = S#state.in_progress - Done,
  NewWorkers = dict:erase(From, S#state.workers),
  {reply, Response, S#state{jobs = NewJobs, in_progress = NeInProgress, done = NewDone, workers = NewWorkers}};

handle_call(stats, _From, S) ->
  Response = {{all_jobs, S#state.all_jobs} , {in_progress, S#state.in_progress}, {done, S#state.done}},
  {reply, Response, S};

handle_call(Request, From, State) ->
  io:format("Server receives unexpeted request~p from ~p~n", [Request, From]),
  {reply, bad_call, State}.

handle_cast(_Request, State) ->  {noreply, State}.

handle_info(_Info, State) ->  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Gets free jobs from list Jobs
%% returns job() | no
get_free_job(Jobs) ->
  JobsLeft = [X || X <- Jobs, X#job.status == free],
  case lists:reverse(JobsLeft) of
    [] -> no;
    [J | _] -> J
  end.

mark_job_in_progress(J, Jobs) when is_record(J, job) ->
  lists:map(fun (Job) ->
                    if J == Job ->
                        J#job{status = in_progress};
                      true ->
                        Job %% return as it is
                    end
            end,
    Jobs);

mark_job_in_progress(_, Jobs) -> Jobs.
%%%===================================================================
%%% TESTS
%%%===================================================================

test_server() ->
  job_center:start_link(),
  J1 = job_center:add_job(fun () -> io:format("Job number 1~n") end),
  J2 = job_center:add_job(fun () -> io:format("Job number 2~n") end),
  J3 = job_center:add_job(fun () -> io:format("Job number 3~n") end),
  io:format("Job numbers: ~p, ~p, ~p~n", [J1, J2, J3]),
  io:format("Job statistics: ~p ~n", [job_center:statistics()]),
  {{all_jobs, 3}, {in_progress, 0}, {done, 0}} = job_center:statistics(),
  {J1, F1} = job_center:work_wanted(),
  F1(),
  {{all_jobs, 3}, {in_progress, 1}, {done, 0}} = job_center:statistics(),
  {J2, F2} = job_center:work_wanted(),
  F2(),
  {{all_jobs, 3}, {in_progress, 2}, {done, 0}} = job_center:statistics(),
  ok = job_center:job_done(J1),
  ok = job_center:job_done(J2),
  {{all_jobs, 3}, {in_progress, 0}, {done, 2}} = job_center:statistics(),
  {error, _} = job_center:job_done(J2),
  {error, _} = job_center:job_done(J3),
  {J3, _} = job_center:work_wanted(),
  {{all_jobs, 3}, {in_progress, 1}, {done, 2}} = job_center:statistics(),
  ok = job_center:job_done(J3),
  {{all_jobs, 3}, {in_progress, 0}, {done, 3}} = job_center:statistics(),
  no = job_center:work_wanted(),
  job_center:add_job(fun () -> io:format("Job number 4~n") end),
  {{all_jobs, 4}, {in_progress, 0}, {done, 3}} = job_center:statistics(),
  io:format("SUCCESS~n"),
  done.




