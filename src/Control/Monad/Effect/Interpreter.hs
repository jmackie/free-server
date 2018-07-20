-- |
-- Module      : Control.Monad.Effect.Interpreter
-- Description : Provides the default 'Effect' interpreter
--
module Control.Monad.Effect.Interpreter
    ( mkInterpreter
    )
where

import           Prelude

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception        (IOException, try)
import           Data.Default             (def)
import           Data.IORef
    ( IORef
    , atomicModifyIORef'
    , atomicWriteIORef
    , newIORef
    , readIORef
    )
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text.IO             as Text
import           Network.HTTP.Req         (responseBody, runReq)
import           System.IO                (stderr)

import           Control.Monad.Effect     (Effect(..), LogLevel(..))


-- | Initialise a new 'Effect' interpreter.
mkInterpreter :: IO (Effect a -> IO a)
mkInterpreter = do
    counter   <- newIORef 0
    _         <- forkIO (manageCounter counter)
    fileCache <- newIORef Map.empty
    pure (interpreter State {..})


manageCounter :: IORef Int -> IO ()
manageCounter ref = do
    threadDelay 1000000
    atomicModifyIORef' ref (\count -> (count + 1, ()))
    manageCounter ref


-- | A bunch of mutable things.
data State = State
    { counter   :: IORef Int
    , fileCache :: IORef (Map FilePath Text)
    }


-- | Default interpreter: runs everything in IO.
interpreter :: State -> Effect a -> IO a
interpreter s = interpret
  where
    interpret = \case
        Pure a               -> pure a

        RequestJSON req cont -> do
            result <- try (runReq def req)
            interpret (cont (responseBody <$> result))

        Log level mesg next -> do
            case level of
                Info  -> Text.putStrLn ("INFO: " <> mesg)
                Error -> Text.hPutStrLn stderr ("ERROR: " <> mesg)
            interpret next

        GetCounter cont -> do
            count <- readIORef (counter s)
            interpret (cont count)

        ReadFile path cont -> do
            result <- cachedReadFile path (fileCache s)
            interpret (cont result)

        Concurrently effects cont -> do
            results <- mapConcurrently (interpreter s) effects
            interpret (cont results)


cachedReadFile
    :: FilePath -> IORef (Map FilePath Text) -> IO (Either IOException Text)
cachedReadFile path ref = do
    cache <- readIORef ref
    case Map.lookup path cache of
        Just contents -> do
            putStrLn ("read cached file: " <> path)
            pure (Right contents)
        Nothing -> do
            result <- try (Text.readFile path)
            case result of
                Left _ -> pure ()
                Right contents ->
                    atomicWriteIORef ref (Map.insert path contents cache)
            pure result
