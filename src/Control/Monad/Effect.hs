{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module      : Control.Monad.Effect
-- Description : All the effects this server has
--
module Control.Monad.Effect
    ( Effect(..)

    -- * HTTP Requests
    , JSON
    , requestJSON


    -- * Stateful Stuff
    , getCounter

    -- * Logging
    , LogLevel(..)
    , logInfo
    , logError

    -- * File System Interaction
    , readFile

    -- * Meta Effects
    , runConcurrently
    )
where

import           Prelude           hiding (log, readFile)

import           Control.Exception (IOException)
import qualified Data.Aeson        as Aeson
import           Data.Text         (Text)
import           Network.HTTP.Req  (HttpException, JsonResponse, Req)


-- | All the effects this server relies on.
--
-- Note that you should only use the value constructors for creating an interpreter.
data Effect a
    = Pure a
    | RequestJSON (Req JSON) (Either HttpException Aeson.Value -> Effect a)
    | Log LogLevel Text (Effect a)
    | GetCounter (Int -> Effect a)
    | ReadFile FilePath (Either IOException Text -> Effect a)
    | forall b . Concurrently [Effect b] ([b] -> Effect a)


-- | A Nicer synonym.
type JSON = JsonResponse Aeson.Value


-- | The interpreter should act differently based on this 'LogLevel'.
data LogLevel = Info | Error


-- | Fire a JSON request.
requestJSON :: Req JSON -> Effect (Either HttpException Aeson.Value)
requestJSON req = RequestJSON req Pure


runConcurrently :: [Effect b] -> Effect [b]
runConcurrently effects = Concurrently effects Pure


-- | Log an info message.
logInfo :: Text -> Effect ()
logInfo = log Info


-- | Log an error message.
logError :: Text -> Effect ()
logError = log Error


log :: LogLevel -> Text -> Effect ()
log level mesg = Log level mesg (Pure ())


-- | Get the state of some counter.
getCounter :: Effect Int
getCounter = GetCounter Pure


-- | Attempt to read a file.
readFile :: FilePath -> Effect (Either IOException Text)
readFile path = ReadFile path Pure


{- INSTANCE BOILERPLATE -}


instance Functor Effect where
    fmap = mapEffect


mapEffect :: (a -> b) -> Effect a -> Effect b
mapEffect f = \case
    Pure a                    -> Pure (f a)
    RequestJSON req cont      -> RequestJSON req (fmap f . cont)
    Log level mesg next       -> Log level mesg (fmap f next)
    GetCounter cont           -> GetCounter (fmap f . cont)
    ReadFile     path    cont -> ReadFile path (fmap f . cont)
    Concurrently effects cont -> Concurrently effects (fmap f . cont)


instance Applicative Effect where
    pure = Pure
    (<*>) = effectApply


effectApply :: Effect (a -> b) -> Effect a -> Effect b
effectApply top e = go top
  where
    go (Pure f                   ) = fmap f e
    go (RequestJSON req cont     ) = RequestJSON req (go . cont)
    go (Log level mesg next      ) = Log level mesg (go next)
    go (GetCounter cont          ) = GetCounter (go . cont)
    go (ReadFile     path    cont) = ReadFile path (go . cont)
    go (Concurrently effects cont) = Concurrently effects (go . cont)


instance Monad Effect where
    return = pure
    (>>=) = effectBind


effectBind :: Effect a -> (a -> Effect b) -> Effect b
effectBind top f = go top
  where
    go (Pure a                   ) = f a
    go (RequestJSON req cont     ) = RequestJSON req (go . cont)
    go (Log level mesg next      ) = Log level mesg (go next)
    go (GetCounter cont          ) = GetCounter (go . cont)
    go (ReadFile     path    cont) = ReadFile path (go . cont)
    go (Concurrently effects cont) = Concurrently effects (go . cont)
