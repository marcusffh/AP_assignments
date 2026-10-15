module SPC_Tests (tests) where

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
    testGroup "SPC (core)"
      [ testCase "worker executes a job" $ do
          spc <- startSPC
          workerResult <- workerAdd spc "worker1"
          case workerResult of
            Left err -> assertFailure err
            Right _ -> do
              jobId <- jobAdd spc (Job (pure ()) 10)
              result <- jobWait spc jobId
              result @?= Just Done

        
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



      ]

