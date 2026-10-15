module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    myThreadId,
    threadDelay,
  )
import Control.Monad (ap, forever, liftM, void)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg --which instructions does a worker understand
  = MsgDoJob JobId (IO ()) -- execute a particular job
  | MsgStopWorker -- tells a worker to stop.

-- Messages sent to SPC.
data SPCMsg -- messages sent to SPC manager
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | -- | Some time has passed.
    MsgTick
  | MsgWorkerDone WorkerName --Task1
  | MsgWorkerAdd WorkerName (ReplyChan (Either String Worker)) -- Task1
  | MsgWorkerGone WorkerName -- Task1

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState -- remember what the manager needs to know
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWorkers :: [(WorkerName, Worker, Maybe JobId)], --Task1: workers and if they are idle or busy
    spcJobWaiters :: [(JobId, [ReplyChan (Maybe JobDoneReason)])] -- Task1: callers waiting for a job to finish
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

-- Task 1 below
schedule :: SPCM () --assign pending jobs to idle workers
schedule = do 
  state <- get
  case (spcJobsPending state, findIdleWorker (spcWorkers state)) of
    ((jobId, job) : pendingJobs, Just (workerName, Worker workerServer, _)) -> do
      io $ sendTo workerServer (MsgDoJob jobId (jobAction job))
      modify $ \s ->
        s
          { spcJobsPending = pendingJobs,
            spcJobsRunning = (jobId, job) : spcJobsRunning s,
            spcWorkers =
              map
                (\(name, worker, currentJob) ->
                   if name == workerName
                     then (name, worker, Just jobId)
                     else (name, worker, currentJob))
                (spcWorkers s)
          }
      schedule
    _ -> pure ()

findIdleWorker
  :: [(WorkerName, Worker, Maybe JobId)]
  -> Maybe (WorkerName, Worker, Maybe JobId)
findIdleWorker [] = Nothing
findIdleWorker (worker@(_, _, Nothing) : _) = Just worker
findIdleWorker (_ : workers) = findIdleWorker workers
-- Task1 above

-- Task1 below
jobDone :: JobId -> JobDoneReason -> SPCM () -- record that a job has finished
jobDone jobId reason = do
  state <- get
  case lookup jobId (spcJobsRunning state) of
    Nothing -> pure ()
    Just _ -> do
      let waiters = maybe [] id (lookup jobId (spcJobWaiters state))
      io $ mapM_ (\rsvp -> reply rsvp (Just reason)) waiters
      modify $ \s ->
        s
          { spcJobsRunning = removeAssoc jobId (spcJobsRunning s),
            spcJobsDone = (jobId, reason) : spcJobsDone s,
            spcJobWaiters = removeAssoc jobId (spcJobWaiters s)
          }
-- Task1 above
-- Task1 below
workerIsIdle :: WorkerName -> Worker -> SPCM () --mark a worker as available again
workerIsIdle name worker = do
  modify $ \state ->
    state
      { spcWorkers =
          map
            (\(workerName, existingWorker, currentJob) ->
               if workerName == name
                 then (workerName, worker, Nothing)
                 else (workerName, existingWorker, currentJob))
            (spcWorkers state)
      }
-- task1 above

-- task1 below
workerIsGone :: WorkerName -> SPCM () -- remove a worker that has stopped
workerIsGone name = do 
  state <- get
  case lookupWorker name (spcWorkers state) of
    Nothing -> pure ()
    Just (_, Just jobId) -> do
      jobDone jobId DoneCancelled
      modify $ \s ->
        s { spcWorkers =
              filter (\(workerName, _, _) -> workerName /= name)
                     (spcWorkers s)
          }
    Just (_, Nothing) ->
      modify $ \s ->
        s { spcWorkers =
              filter (\(workerName, _, _) -> workerName /= name)
                     (spcWorkers s)
          }
-- task1 above

checkTimeouts :: SPCM ()
checkTimeouts = pure () -- change in Task 4

workerExists :: WorkerName -> SPCM Bool -- check whether a worker name is already registered
workerExists name = do 
  state <- get
  pure $ any (\(workerName, _, _) -> workerName == name) 
              (spcWorkers state) --task1

-- helper for task1: finding and inspecting a specific worker
lookupWorker --find worker by name and returns its worker object and current job
  :: WorkerName
  -> [(WorkerName, Worker, Maybe JobId)]
  -> Maybe (Worker, Maybe JobId)
lookupWorker _ [] = Nothing
lookupWorker name ((workerName, worker, currentJob) : workers)
  | name == workerName = Just (worker, currentJob)
  | otherwise = lookupWorker name workers

-- helper for task1: allows multipe callers to wait for the same job
addWaiter
  :: JobId -- adds a caller to the list of callers waiting for a particular job
  -> ReplyChan (Maybe JobDoneReason)
  -> [(JobId, [ReplyChan (Maybe JobDoneReason)])]
  -> [(JobId, [ReplyChan (Maybe JobDoneReason)])]
addWaiter jobId rsvp [] = [(jobId, [rsvp])]
addWaiter jobId rsvp ((existingId, waiters) : rest)
  | jobId == existingId = (existingId, rsvp : waiters) : rest
  | otherwise = (existingId, waiters) : addWaiter jobId rsvp rest

-- helper for task1: helps determine whether a job is still active
isJobPendingOrRunning :: JobId -> SPCState -> Bool --checks whether a job is still pending or running
isJobPendingOrRunning jobId state =
  maybe False (const True) (lookup jobId (spcJobsPending state))
    || maybe False (const True) (lookup jobId (spcJobsRunning state))


-- helper for task1: ensures callers are informed when the job finishes
notifyJobWaiters :: JobId -> JobDoneReason -> SPCM () -- sends the final result to callers waiting for a job
notifyJobWaiters jobId reason = do
  state <- get
  let waiters = maybe [] id (lookup jobId (spcJobWaiters state))
  io $ mapM_ (\rsvp -> reply rsvp (Just reason)) waiters
  modify $ \s ->
    s { spcJobWaiters = removeAssoc jobId (spcJobWaiters s) }


handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> Just JobPending
        (_, Just _, _) -> Just JobRunning
        (_, _, Just r) -> Just $ JobDone r
        _ -> Nothing

    -- task1 below: make SPC respond to the new message types
    MsgWorkerAdd name rsvp -> do --register a new worker
      exists <- workerExists name
      if exists
        then io $ reply rsvp (Left "Worker name already exists")
        else do
          workerServer <- io $ spawn (workerLoop c name)
          let worker = Worker workerServer
          modify $ \state ->
            state { spcWorkers = (name, worker, Nothing) : spcWorkers state }
          io $ reply rsvp (Right worker)
    -- task1 above

    -- Task1 below
    MsgWorkerDone name -> do -- handle a worker reporting that its job has finished
      state <- get
      case lookupWorker name (spcWorkers state) of
        Nothing -> pure ()
        Just (worker, Just jobId) -> do
          jobDone jobId Done
          workerIsIdle name worker
        Just (_, Nothing) -> pure ()
    -- Task1 above
    
    -- Task1 below
    MsgJobWait jobId rsvp -> do -- handle a request to wait for a job
      state <- get
      case lookup jobId (spcJobsDone state) of
        Just reason ->
          io $ reply rsvp (Just reason)
        Nothing ->
          if isJobPendingOrRunning jobId state
            then
              modify $ \s ->
                s
                  { spcJobWaiters =
                      addWaiter jobId rsvp (spcJobWaiters s)
                  }
            else
              io $ reply rsvp Nothing
    -- Task1 above

    -- Task1 below
    MsgWorkerGone name -> -- handle a worker stopping
      workerIsGone name
    -- Task1 above


    --Task1 below
    MsgJobCancel jobId -> do -- handle a request to cancel a job
      state <- get
      case lookup jobId (spcJobsPending state) of
        Just _ -> do
          modify $ \s ->
            s { spcJobsPending = removeAssoc jobId (spcJobsPending s) }
          modify $ \s ->
            s { spcJobsDone = (jobId, DoneCancelled) : spcJobsDone s }
          notifyJobWaiters jobId DoneCancelled
        Nothing -> pure ()
    --task1 above

-- Task1 below : keep a worker alive so it ca execute multiple jobs
workerLoop :: Chan SPCMsg -> WorkerName -> Chan WorkerMsg -> IO ()
workerLoop spcChan name workerChan = do 
  msg <- receive workerChan
  case msg of
    MsgDoJob jobId action -> do 
      action
      send spcChan (MsgWorkerDone name)
      workerLoop spcChan name workerChan
    MsgStopWorker -> do 
      send spcChan (MsgWorkerDone name)
-- Task1 above


startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkers = [], -- Task1
            spcJobWaiters = [] -- Task1
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
-- task1 below
workerAdd :: SPC -> WorkerName -> IO (Either String Worker) -- provide the public API for adding a worker
workerAdd (SPC spcServer) name = do 
  result <- requestReply spcServer $ \replyChan ->
    MsgWorkerAdd name replyChan
  pure result
-- task1 above

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
-- task1 below
workerStop :: Worker -> IO () -- provide the public API for stopping a worker
workerStop (Worker workerServer) =
  sendTo workerServer MsgStopWorker
-- task1 above
