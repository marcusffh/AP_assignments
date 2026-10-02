module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)


-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.


data Stop = Err Error | Brk Val -- part 4 error vs break


runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  res <- runEvalIO' envEmpty dbFile evalm -- part 4 below
  pure $ case res of
    Left (Err e) -> Left e
    Left (Brk _) -> Left "Break outside loop"
    Right x -> Right x -- part 4 above
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Stop a) -- part 4
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left (Err e) -- part 4
    runEvalIO' r db (Free (TryCatchOp e1 e2 k)) = do -- part 1
      result <- runEvalIO' r db e1
      case result of
        Left (Err _) -> runEvalIO' r db (e2 >>= k) -- part 4: only errors are caught
        Left (Brk v) -> pure $ Left (Brk v) -- part 4: break passes through
        Right v -> runEvalIO' r db (k v)

    runEvalIO' r db (Free (KvGetOp key k)) = do 
        result <- readDB db
        case result of
          Left e -> pure $ Left (Err e) -- part 4
          Right dbState -> 
            case lookup key dbState of 
              Nothing -> do 
                input <- prompt $ "Invalid key: " ++ show key ++ ". Enter a replacement: "
                case readVal input of 
                  Just val -> runEvalIO' r db (k val)
                  Nothing -> pure $ Left (Err ("Invalid value input: " ++ input)) -- part 4
              
              Just val -> runEvalIO' r db (k val)
  
    runEvalIO' r db (Free (KvPutOp key val k)) = do
      result <- readDB db
      case result of 
        Left e -> pure $ Left (Err e) -- part 4
        Right dbState -> do 
          let newState = (key, val) : filter (\(oldkey, _) -> oldkey /= key) dbState
          writeDB db newState
          runEvalIO' r db k 

    -- part 3 below
    runEvalIO' r db (Free (TransactionOp m k)) = do
      res <- withTempDB $ \tmp -> do
        copyDB db tmp                 -- work on a copy of the database
        out <- runEvalIO' r tmp m     -- run the body against the copy
        case out of
          Right v -> copyDB tmp db >> pure (Right v)  -- success: commit
          Left e -> pure (Left e)                      -- failure: discard
      case res of
        Left e -> pure (Left e)
        Right v -> runEvalIO' r db (k v)
    -- part 3 above
    -- part 4 below
    runEvalIO' _ _ (Free (BreakOp v)) = pure $ Left (Brk v)
    runEvalIO' r db (Free (LoopOp m k)) = do
      result <- runEvalIO' r db m
      case result of
        Right v -> runEvalIO' r db (k v) -- body finished normally
        Left (Brk v) -> runEvalIO' r db (k v) -- break: the loop returns v
        Left (Err e) -> pure $ Left (Err e) -- errors propagate
    -- part 4 above
