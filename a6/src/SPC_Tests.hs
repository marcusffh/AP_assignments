module SPC_Tests (tests) where

import Control.Exception (throwIO)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- test for task1 
tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup "SPC (core)" $
      [ testCase "worker executes a job" $ do
          spc <- startSPC
          workerResult <- workerAdd spc "worker1"
          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              jobId <- jobAdd spc (Job (pure ()) 10)
              result <- jobWait spc jobId
              result @?= Just Done

        -- Two workers can execute two jobs and that both jobs finish successfully
      , testCase "two workers execute jobs concurrently" $ do
          spc <- startSPC
          firstWorker <- workerAdd spc "worker1"
          secondWorker <- workerAdd spc "worker2"

          case (firstWorker, secondWorker) of
            (Right _, Right _) -> do
              ref <- newIORef (0 :: Int)

              let action = do
                    threadDelay 200000
                    modifyIORef ref (+ 1)
              
              firstJob <- jobAdd spc (Job action 10)
              secondJob <- jobAdd spc (Job action 10)

              firstResult <- jobWait spc firstJob
              secondResult <- jobWait spc secondJob
             

              firstResult @?= Just Done
              secondResult @?= Just Done

              readIORef ref >>= (@?= 2)

            (Left err, _) -> assertFailure err
            (_, Left err) -> assertFailure err


        -- SPC refuses to register two workers with the same name
      , testCase "duplicate worker name is rejected" $ do
          spc <- startSPC
          first <- workerAdd spc "worker1"
          second <- workerAdd spc "worker1"

          case first of
            Left err -> assertFailure err
            Right _ ->
              case second of
                Left _ -> pure ()
                Right _ -> assertFailure "Duplicate worker name was accepted"


      -- A job waiting in the pending queue can be cancelled without being executed
      , testCase "pending job can be cancelled" $ do
          spc <- startSPC
          workerResult <- workerAdd spc "worker1"

          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              runningJob <- jobAdd spc (Job (threadDelay 500000) 10)
              pendingJob <- jobAdd spc (Job (pure ()) 10)

              -- Wait until the first job has started.
              let waitUntilRunning 0 = assertFailure "Job never started"
                  waitUntilRunning n = do
                    status <- jobStatus spc runningJob
                    case status of
                      Just JobRunning -> pure ()
                      _ -> threadDelay 10000 >> waitUntilRunning (n - 1)

              waitUntilRunning (50 :: Int)
              jobCancel spc pendingJob

              result <- jobWait spc pendingJob
              result @?= Just DoneCancelled


      -- A single worker can execute one job, finish it, and then execute another job
      , testCase "worker executes multiple jobs sequentially" $ do
          spc <- startSPC
          workerResult <- workerAdd spc "worker1"

          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              ref <- newIORef (0 :: Int)

              let action = modifyIORef ref (+ 1)

              firstJob <- jobAdd spc (Job action 10)
              firstResult <- jobWait spc firstJob
              firstResult @?= Just Done

              secondJob <- jobAdd spc (Job action 10)
              secondResult <- jobWait spc secondJob
              secondResult @?= Just Done

              readIORef ref >>= (@?= 2)


        -- test for task1
      , testCase "pending job runs when a worker is added later" $ do
          spc <- startSPC

          jobId <- jobAdd spc (Job (pure ()) 10)

          status <- jobStatus spc jobId
          status @?= Just JobPending

          workerResult <- workerAdd spc "worker1"

          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              result <- jobWait spc jobId
              result @?= Just Done

      -- test for for task2
      , testCase "running job can be canelled and worker reused" $ do 
          spc <- startSPC
          workerResult <- workerAdd spc "worker1"

          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              runningJob <- jobAdd spc (Job (threadDelay 5000000) 10)

              -- Wait until the job has actually started.
              let waitUntilRunning 0 =
                    assertFailure "Job never started"
                  waitUntilRunning n = do
                    status <- jobStatus spc runningJob
                    case status of
                      Just JobRunning -> pure ()
                      _ -> threadDelay 10000 >> waitUntilRunning (n - 1)

              waitUntilRunning (50 :: Int)

              -- Cancel the running job.
              jobCancel spc runningJob
              result <- jobWait spc runningJob
              result @?= Just DoneCancelled

              -- The same worker should now be able to run another job.
              nextJob <- jobAdd spc (Job (pure ()) 10)
              nextResult <- jobWait spc nextJob
              nextResult @?= Just Done
    
      , testCase "timed-out job is terminated and worker is reused" $ do
          spc <- startSPC
          workerResult <- workerAdd spc "timeout-worker"

          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              timedJob <- jobAdd spc (Job (threadDelay 5000000) 1)

              result <- jobWait spc timedJob
              result @?= Just DoneTimeout

              nextJob <- jobAdd spc (Job (pure ()) 10)
              nextResult <- jobWait spc nextJob
              nextResult @?= Just Done




      , testCase "crashed job is recorded and worker is reused" $ do
          spc <- startSPC
          workerResult <- workerAdd spc "crash-worker"

          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              crashedJob <-
                jobAdd spc (Job (throwIO (userError "job crashed")) 10)

              result <- jobWait spc crashedJob
              result @?= Just DoneCrashed

              nextJob <- jobAdd spc (Job (pure ()) 10)
              nextResult <- jobWait spc nextJob
              nextResult @?= Just Done

      
    

        

      ]
