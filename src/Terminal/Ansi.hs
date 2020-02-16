{-# LANGUAGE OverloadedStrings #-}

module Terminal.Ansi
  ( red,
    green,
    bold,
  )
where

import Data.Text (Text)

green :: Text -> Text
green t = greenCode <> t <> resetCode

red :: Text -> Text
red t = redCode <> t <> resetCode

bold :: Text -> Text
bold t = boldCode <> t <> resetCode

greenCode, redCode, resetCode, boldCode :: Text
greenCode = "\ESC[32m"
redCode = "\ESC[31m"
resetCode = "\ESC[0m"
boldCode = "\ESC[1m"
