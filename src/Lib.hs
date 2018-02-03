{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runServer
    , fetchSid
    , fetchToken
    ) where

import Control.Monad.Except (throwError, liftIO)
import Data.Aeson
import Data.Proxy (Proxy(..))
import Data.Text
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment (getEnv)
import Twilio
import Twilio.Messages

type MyAPI = "api" :> "hello" :> Get '[JSON] String
  :<|> "api" :> "sms" :> ReqBody '[JSON] PostMessage :> Post '[JSON] ()

instance FromJSON PostMessage where
  parseJSON = withObject "Post Message" $ \o -> do
    return $ PostMessage "" "" ""

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

helloHandler :: Twilio String
helloHandler = return "Hello World!"

smsHandler :: PostMessage -> Twilio ()
smsHandler msg = liftIO $ print msg
  {-print (sendTo msg)
  print (sendFrom msg)
  print (sendBody msg)-}

transformToHandler :: Twilio :~> Handler
transformToHandler = NT $ \action -> liftIO $ runTwilio' fetchSid fetchToken action

fetchSid :: IO String
fetchSid = (getEnv "TWILIO_ACCOUNT_SID")

fetchToken :: IO String
fetchToken = (getEnv "TWILIO_AUTH_TOKEN")

endServer :: Server MyAPI
endServer = enter transformToHandler $
  helloHandler :<|>
  smsHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  run port (serve myAPI endServer)
