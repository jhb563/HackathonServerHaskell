{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( runServer
    ) where

import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment (getEnv)

type MyAPI = "api" :> "hello" :> Get '[JSON] String

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myAPIHandler :: Handler String
myAPIHandler = return "Hello World!"

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve myAPI myAPIHandler)
