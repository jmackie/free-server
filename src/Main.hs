-- |
-- Module      : Main
-- Description : Spins up the server
--
module Main (main) where

import           Prelude

import           Control.Monad.Effect.Interpreter (mkInterpreter)
import qualified Handler
import           Network.Wai.Handler.Warp         (run)


port :: Int
port = 8888


main :: IO ()
main = do
    interpreter <- mkInterpreter
    putStrLn ("serving on port " <> show port)
    run port $ \request respond -> do
        response <- interpreter (Handler.response request)
        respond response
