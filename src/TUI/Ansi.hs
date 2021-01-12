{-# LANGUAGE OverloadedStrings #-}

module TUI.Ansi (
    cliInfo,
    cliWarn,
) where

import qualified Data.Text.IO as Text

import Data.Text (Text)


cliInfo, cliWarn :: Text -> IO ()
cliInfo msg = Text.putStrLn $ green msg
cliWarn msg = Text.putStrLn $ red msg


green :: Text -> Text
green t = greenCode <> t <> resetCode


red :: Text -> Text
red t = redCode <> t <> resetCode


greenCode, redCode, resetCode :: Text
greenCode = "\ESC[32m"
redCode = "\ESC[31m"
resetCode = "\ESC[0m"
