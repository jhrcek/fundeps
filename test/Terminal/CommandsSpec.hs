{-# LANGUAGE OverloadedStrings #-}

module Terminal.CommandsSpec where

import qualified Data.GraphViz.Commands as Gv
import Terminal.Commands (Command (..), CommandParseError (..), QueryItem (..), parseCommand)
import Test.Hspec


spec :: Spec
spec =
    describe "parseCommand" $ do
        it "should parse empty input" $
            parseCommand "" `shouldBe` Right NoOp
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
            parseCommand ":export file.dot map" `shouldBe` Right (Export "file.dot" Gv.Canon [Fun "map"])
            parseCommand ":export file.svg map" `shouldBe` Right (Export "file.svg" Gv.Svg [Fun "map"])
        it "shouldn't parse invalid export format" $ do
            parseCommand ":export file.png map"
                `shouldBe` Left (CommandParseError "\":export file.png map\" (line 1, column 18):\nunexpected \"m\"\nexpecting space\nExpected format of export command is :export FILE.(dot|svg) QUERY")
        it "shouldn't parse incomplete export command" $ do
            parseCommand ":export"
                `shouldBe` Left (CommandParseError "\":export\" (line 1, column 8):\nunexpected end of input\nexpecting white space")
            parseCommand ":export map"
                `shouldBe` Left (CommandParseError "\":export map\" (line 1, column 12):\nunexpected end of input\nexpecting white space\nExpected format of export command is :export FILE.(dot|svg) QUERY")
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
