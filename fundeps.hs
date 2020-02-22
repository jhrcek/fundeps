#!/usr/bin/env stack
{- stack script
   --resolver lts-14.27
   --package turtle
   --package process
-}

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
  withElmStuffBackup $
    procs "hacked-elm" ("make" : "--output=/dev/null" : filesToCompile) empty
  output "tmp" $ do
    usagesFile <- getUsages
    input usagesFile
  sh $ rm =<< getUsages
  mv "tmp" "all.usages"
  sh $ liftIO $ callProcess "fundeps-exe" []
  rm "all.usages"

withElmStuffBackup :: IO () -> IO ()
withElmStuffBackup action = do
  elmStuffExists <- testdir stuff
  if elmStuffExists
    then withBackup action
    else action
  where
    withBackup act = with (mktempdir "/tmp" "fundeps") $ \tmpDir ->
      bracket_
        -- TODO figure out how to do it via `mv`, which should be much faster
        -- TODO or better yet hack elm so it completely ignores local project cache and always recompiles the files
        (cptree stuff (tmpDir </> stuff) >> rmtree stuff)
        (rmtree stuff >> cptree (tmpDir </> stuff) stuff)
        act

getUsages :: Shell Turtle.FilePath
getUsages = mfilter (\f -> extension f == Just "usages") (ls ".")

parseArgs :: IO [Text]
parseArgs =
  options "Script to run fundeps"
    $ some
    $ argText "FILE(s)" "One or more elm files to compile"

stuff :: Turtle.FilePath
stuff = "elm-stuff/"
