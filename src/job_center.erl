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
-export([start_link/0, add_job/1, work_wanted/0, job_done/1, test_server/0, statistics/0, stop/0]).

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
                worker  :: reference(),
                time    :: integer() , %% time in seconds to complete the job by worker, default=0
                hurry_up_timer   :: tref(),  %% timer ref to hurry_up
                timer   :: tref()            %% Timer reference for work expiration
}).

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

stop() ->
  gen_server:stop(?SERVER).

add_job(F) ->
  {ok, JobNumber} = gen_server:call(?SERVER, {add_job, F}),
  JobNumber.

%%  returns {JobNumber, JobTime, F} | no.
%% where JobTime is a time in seconds that the worker has to complete the job by.
work_wanted() ->
  J = gen_server:call(?SERVER, work_wanted),
  case J of
    no -> no;
    Job -> {Job#job.id, Job#job.time, Job#job.f}
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

handle_call(work_wanted, {WorkerPid, _Tag} = _From, S) ->
  FreeJob = get_free_job(S#state.jobs),
  case FreeJob of
    no ->
      {reply, no, S};
    Job ->
      Time = get_time_for_job(),
      HurryUpTimeRef = erlang:send_after(Time - 1, WorkerPid, hurry_up),
      ExpirationTimeRef = erlang:send_after(Time + 1, self(), {work_expired, WorkerPid, Job#job.id}),
      NewJob = Job#job{status = in_progress, time = Time, hurry_up_timer = HurryUpTimeRef, timer = ExpirationTimeRef},
      %% if we have free job give it to worker and monitor him
      monitor(process, WorkerPid),
      NewJobs = replace_job(NewJob, S#state.jobs),
      NewInProgress = S#state.in_progress + (if Job == no -> 0; true -> 1 end),
      NewState = S#state{jobs = NewJobs, in_progress = NewInProgress, workers = dict:store(WorkerPid, Job, S#state.workers)},
      {reply, Job, NewState}
  end;

handle_call({job_done, JobNumber}, {Pid, _Tag} = _From, S) ->
  Completed = [X || X <- S#state.jobs, X#job.id == JobNumber, X#job.status == in_progress],
  {Response, NewJobs} = case Completed of
                        [E] ->
                          erlang:cancel_timer(E#job.hurry_up_timer),
                          erlang:cancel_timer(E#job.timer),
                          {ok, lists:delete(E, S#state.jobs)};
                        [] -> {{error, bad_job_number}, S#state.jobs}
                    end,
  Done = (if Response == {error, bad_job_number} -> 0; true -> 1 end),
  NewDone = S#state.done + Done,
  NeInProgress = S#state.in_progress - Done,
  NewWorkers = dict:erase(Pid, S#state.workers),
  {reply, Response, S#state{jobs = NewJobs, in_progress = NeInProgress, done = NewDone, workers = NewWorkers}};

handle_call(stats, _From, S) ->
  Response = {{all_jobs, S#state.all_jobs} , {in_progress, S#state.in_progress}, {done, S#state.done}},
  {reply, Response, S};

handle_call(Request, From, State) ->
  io:format("Server receives unexpeted request~p from ~p~n", [Request, From]),
  {reply, bad_call, State}.

handle_cast(_Request, State) ->  {noreply, State}.

handle_info({'DOWN', _Ref, _Type, Pid, Reason}, S) ->
  Job = dict:fetch(Pid, S#state.workers),
  erlang:cancel_timer(Job#job.hurry_up_timer),
  erlang:cancel_timer(Job#job.timer),
  NewJob = Job#job{status = free, time = 0},
  NewWorkers = dict:erase(Pid, S#state.workers),
  NewJobs = replace_job(NewJob, S#state.jobs),
  io:format("Worker ~p died because of ~p, returning his work to pool ~p ~n", [Pid, Reason, Job]),
  {noreply, S#state{jobs = NewJobs, in_progress = S#state.in_progress - 1, workers = NewWorkers}};

handle_info({work_expired, WorkerPid, _JobId}, S) ->
  exit(WorkerPid, youre_fired),
  {noreply, S};

handle_info(Info, State) ->
  io:format("Unexpected Handle Info message ~p ~n", [Info]),
  {noreply, State}.

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

%% replace jobs in lists
replace_job(NewJob, Jobs) when is_record(NewJob, job) ->
  lists:map(fun (Job) ->
                    if NewJob#job.id == Job#job.id ->
                      NewJob;
                      true ->
                        Job %% return as it is
                    end
            end,
    Jobs).

%% in seconds
get_time_for_job() -> 3.

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
  {J1, _Time, F1} = job_center:work_wanted(),
  F1(),
  {{all_jobs, 3}, {in_progress, 1}, {done, 0}} = job_center:statistics(),
  {J2, _Time, F2} = job_center:work_wanted(),
  F2(),
  {{all_jobs, 3}, {in_progress, 2}, {done, 0}} = job_center:statistics(),
  ok = job_center:job_done(J1),
  ok = job_center:job_done(J2),
  {{all_jobs, 3}, {in_progress, 0}, {done, 2}} = job_center:statistics(),
  {error, _} = job_center:job_done(J2),
  {error, _} = job_center:job_done(J3),
  {J3, _, _} = job_center:work_wanted(),
  {{all_jobs, 3}, {in_progress, 1}, {done, 2}} = job_center:statistics(),
  ok = job_center:job_done(J3),
  {{all_jobs, 3}, {in_progress, 0}, {done, 3}} = job_center:statistics(),
  no = job_center:work_wanted(),
  job_center:add_job(fun () -> io:format("Job number 4~n") end),
  {{all_jobs, 4}, {in_progress, 0}, {done, 3}} = job_center:statistics(),
  io:format("SUCCESS Basic tests~n"),
  job_center:stop(),
  test_monitor_workers(),
  io:format("SUCCESS Monitoring Workers ~n"),
  done.

test_monitor_workers() ->
  job_center:start_link(),
  _J1 = job_center:add_job(fun () -> io:format("Job number 1~n") end),
  _J2 = job_center:add_job(fun () -> io:format("Job number 2~n") end),
  io:format("Job statistics: ~p ~n", [job_center:statistics()]),
  W1 = spawn(fun () ->
                  io:format(" Worker with alcohol problems started. Will never finish his work. Because he is always drunk. May die. ~n"),
                  job_center:work_wanted(),
                  receive
                    vodka ->
                      io:format("Thnak you for vodka~n"),
                      exit("Im drunk")
                  end
             end),
  timer:sleep(200),
  io:format("Job statistics before workere die: ~p ~n", [job_center:statistics()]),
  {{all_jobs, 2}, {in_progress, 1}, {done, 0}} = job_center:statistics(),
  W1 ! vodka,
  io:format("Job statistics after worker die: ~p ~n", [job_center:statistics()]),
  timer:sleep(200),
  {{all_jobs, 2}, {in_progress, 0}, {done, 0}} = job_center:statistics(),
  job_center:stop(),
  done.

