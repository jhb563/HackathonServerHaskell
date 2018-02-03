{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Twilio
import Twilio.Messages

main :: IO ()
main = do
  fetchSid >>= print
  fetchToken >>= print
  runTwilio' fetchSid fetchToken $ do
    let body = PostMessage "+14158706220" "+15715703622" "Sending message"
    _ <- post body
    return ()
