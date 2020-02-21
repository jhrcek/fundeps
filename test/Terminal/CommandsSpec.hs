{-# LANGUAGE OverloadedStrings #-}

module Terminal.CommandsSpec where

import Data.Function ((&))
import Terminal.Commands (Command (..), ExportFormat (..), QueryItem (..), parseCommand)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isLeft)

spec :: Spec
spec =
  describe "parseCommand" $ do
    it "should parse help" $
      parseCommand ":help" `shouldBe` Right ShowHelp
    it "should parse graph" $
      parseCommand ":graph" `shouldBe` Right ShowGraph
    it "should parse graph with spaces" $
      parseCommand ":graph   " `shouldBe` Right ShowGraph
    it "should parse quit" $ do
      parseCommand ":quit" `shouldBe` Right Quit
      parseCommand ":q" `shouldBe` Right Quit
    it "should parse settings" $ do
      parseCommand ":set" `shouldBe` Right EditSettings
      parseCommand ":settings" `shouldBe` Right EditSettings
    it "should parse export" $ do
      parseCommand ":export dot map" `shouldBe` Right (Export DotSource [Fun "map"])
      parseCommand ":export svg map" `shouldBe` Right (Export Svg [Fun "map"])
    it "shouldn't parse invalid export format" $ do
      parseCommand ":export png map" & isLeft
    it "shouldn't parse incomplete export command" $
      parseCommand ":export" & isLeft
    describe "query parsing" $ do
      it "should parse function query" $
        parseCommand "map" `shouldBe` Right (Query [Fun "map"])
      it "should parse function query followed by spaces" $
        parseCommand "map   " `shouldBe` Right (Query [Fun "map"])
      it "should parse functions with underscores" $
        parseCommand "A:f_" `shouldBe` Right (Query [ModFun "A" "f_"])
      it "should parse multiple query items" $
        parseCommand "map,someFunction, other"
          `shouldBe` Right (Query [Fun "map", Fun "someFunction", Fun "other"])
      it "should parse multiple complex query items" $
        parseCommand "map,List.Extra:map5,author/package:Extra.Long.Module.Name:functionWithNumber0"
          `shouldBe` Right (Query [Fun "map", ModFun "List.Extra" "map5", PkgModFun "author/package" "Extra.Long.Module.Name" "functionWithNumber0"])
      it "should parse items with dashes" $
        parseCommand "elm-community/maybe-extra:Maybe.Extra:andMap" `shouldBe` Right (Query [PkgModFun "elm-community/maybe-extra" "Maybe.Extra" "andMap"])
      it "should parse module:function query" $
        parseCommand "List:map" `shouldBe` Right (Query [ModFun "List" "map"])
      it "should parse package:module:function query" $
        parseCommand "elm/core:List:map"
          `shouldBe` Right (Query [PkgModFun "elm/core" "List" "map"])
