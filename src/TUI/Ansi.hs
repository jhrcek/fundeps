{-# LANGUAGE OverloadedStrings #-}

module TUI.Ansi
    ( cliInfo
    , cliWarn
    )
where

import Data.Text (Text)
import Data.Text.IO qualified as Text


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
