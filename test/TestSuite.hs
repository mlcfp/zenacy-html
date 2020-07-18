--------------------------------------------------------------------------------
-- Copyright (C) 2016 Zenacy Reader Technologies LLC
--------------------------------------------------------------------------------

module Main where

import qualified Zenacy.HTML.Internal.Buffer.Tests
import qualified Zenacy.HTML.Internal.Entity.Tests
import qualified Zenacy.HTML.Internal.HTML.Tests
import qualified Zenacy.HTML.Internal.Image.Tests
import qualified Zenacy.HTML.Internal.Lexer.Tests
import qualified Zenacy.HTML.Internal.Oper.Tests
import qualified Zenacy.HTML.Internal.Parser.Tests
import qualified Zenacy.HTML.Internal.Token.Tests
import Zenacy.HTML.Internal.Trie.Tests
import qualified Zenacy.HTML.Internal.Zip.Tests
import qualified Zenacy.HTML.Query.Tests

import Test.Framework
  ( defaultMain
  , testGroup
  )

main :: IO ()
main = defaultMain
  [ testTrie
  , testGroup
      "Zenacy.HTML.Internal.Buffer.Tests"
      Zenacy.HTML.Internal.Buffer.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Token.Tests"
      Zenacy.HTML.Internal.Token.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Entity.Tests"
      Zenacy.HTML.Internal.Entity.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Lexer.Tests"
      Zenacy.HTML.Internal.Lexer.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Parser.Tests"
      Zenacy.HTML.Internal.Parser.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.HTML.Tests"
      Zenacy.HTML.Internal.HTML.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Oper.Tests"
      Zenacy.HTML.Internal.Oper.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Zip.Tests"
      Zenacy.HTML.Internal.Zip.Tests.tests
  , testGroup
      "Zenacy.HTML.Internal.Image.Tests"
      Zenacy.HTML.Internal.Image.Tests.tests
  , testGroup
      "Zenacy.HTML.Query.Tests"
      Zenacy.HTML.Query.Tests.tests
  ]
