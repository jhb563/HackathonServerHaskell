module Main where

import System.Environment (getEnv)

import Lib

main :: IO ()
main = do
  getEnv "TWILIO_ACCOUNT_SID" >>= print
  getEnv "TWILIO_AUTH_TOKEN" >>= print
  runServer
