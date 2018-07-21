{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
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
import           Data.Kind         (Type)
import           Data.Text         (Text)
import           Network.HTTP.Req  (HttpException, JsonResponse, Req)


-- | All the effects this server relies on.
--
-- Note that you should only use the value constructors for creating an interpreter.
data Effect (result :: Type)
    = Pure result
    | RequestJSON (Req JSON) (Either HttpException Aeson.Value -> Effect result)
    | Log LogLevel Text (Effect result)
    | GetCounter (Int -> Effect result)
    | ReadFile FilePath (Either IOException Text -> Effect result)
    | forall b . Concurrently [Effect b] ([b] -> Effect result)


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


mapEffect :: (a -> b) -> Effect a -> Effect b
mapEffect f = \case
    Pure a                    -> Pure (f a)
    RequestJSON req cont      -> RequestJSON req (mapEffect f . cont)
    Log level mesg next       -> Log level mesg (mapEffect f next)
    GetCounter cont           -> GetCounter (mapEffect f . cont)
    ReadFile     path    cont -> ReadFile path (mapEffect f . cont)
    Concurrently effects cont -> Concurrently effects (mapEffect f . cont)


extendEffect :: (a -> Effect b) -> Effect a -> Effect b
extendEffect f = \case
    Pure a                    -> f a
    RequestJSON req cont      -> RequestJSON req (extendEffect f . cont)
    Log level mesg next       -> Log level mesg (extendEffect f next)
    GetCounter cont           -> GetCounter (extendEffect f . cont)
    ReadFile     path    cont -> ReadFile path (extendEffect f . cont)
    Concurrently effects cont -> Concurrently effects (extendEffect f . cont)


instance Functor Effect where
    fmap = mapEffect


instance Applicative Effect where
    pure = Pure
    ef <*> e = extendEffect (\f -> fmap f e) ef


instance Monad Effect where
    return = pure
    (>>=) = flip extendEffect
