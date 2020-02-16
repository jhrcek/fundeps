#!/usr/bin/env stack
-- stack script --resolver lts-14.25 --package turtle --package process

{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import System.Process
import Turtle

main :: IO ()
main = do
  filesToCompile <- parseArgs
  elmJsonExists <- testfile "elm.json"
  unless elmJsonExists $
    die "elm.json not found in current directory. This doesn't look like elm project."
  elmStuffExists <- testdir stuff
  when elmStuffExists
    $ with (mktempdir "/tmp" "fundeps")
    $ \tmpDir -> do
      bracket_
        (cptree stuff (tmpDir </> stuff) >> rmtree stuff)
        (rmtree stuff >> cptree (tmpDir </> stuff) stuff)
        (procs "hacked-elm" ("make" : "--output=/dev/null" : filesToCompile) empty)
  output "tmp" $ do
    usagesFile <- getUsages
    input usagesFile
  sh $ rm =<< getUsages
  mv "tmp" "all.usages"
  sh $ liftIO $ callProcess "fundeps-exe" []
  rm "all.usages"

getUsages :: Shell Turtle.FilePath
getUsages = mfilter (\f -> extension f == Just "usages") (ls ".")

parseArgs :: IO [Text]
parseArgs =
  options "Script to run fundeps"
    $ some
    $ argText "FILE(s)" "One or more elm files to compile"

stuff :: Turtle.FilePath
stuff = "elm-stuff/"
