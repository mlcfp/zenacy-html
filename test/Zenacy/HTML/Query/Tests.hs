--------------------------------------------------------------------------------
-- Copyright (C) 2016 Zenacy Reader Technologies LLC
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Query.Tests
  ( testQuery
  ) where

import Zenacy.HTML
import qualified Zenacy.HTML.Query as Q
import Data.Maybe
  ( fromJust
  )
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )

testQuery :: Test
testQuery = testGroup "Zenacy.HTML.Query"
  [ testFirst
  , testLast
  , testNext
  , testPrev
  , testUp
  , testIsFirst
  , testIsLast
  , testSave
  , testNode
  , testName
  , testAttr
  , testAttrVal
  , testId
  , testClass
  , testGeneral
  , testOnly
  ]

h = htmlParseEasy
    "<h1></h1><p>\
    \<a href='bbb'>AAA</a><span id='x' class='y z'></span><br><img>\
    \</p>"
b = fromJust $ htmlDocBody h

testFirst :: Test
testFirst = testCase "query first" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.first >> Q.name "h1" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.first >> Q.first >> Q.succ True

testLast :: Test
testLast = testCase "query last" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.last >> Q.name "p" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.first >> Q.last >> Q.succ True

testNext :: Test
testNext = testCase "query next" $ do
  assertEqual "TEST 1" (Nothing) $
    Q.run b $ Q.next >> Q.succ True

testPrev :: Test
testPrev = testCase "query prev" $ do
  assertEqual "TEST 1" (Nothing) $
    Q.run b $ Q.prev >> Q.succ True

testUp :: Test
testUp = testCase "query up" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.last >> Q.last >> Q.name "img"
      >> Q.up >> Q.name "p" >> Q.up >> Q.name "body"
      >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.up >> Q.succ True

testIsFirst :: Test
testIsFirst = testCase "query isfirst" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.isFirst >> Q.succ True

testIsLast :: Test
testIsLast = testCase "query islast" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.isLast >> Q.succ True

testSave :: Test
testSave = testCase "query save" $ do
  assertEqual "TEST 1" (Just "body") $
    Q.run b $ Q.save 4 >> Q.get 4 >>= \a -> Q.succ $ htmlElemName a

testNode :: Test
testNode = testCase "query node" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.node >>= Q.succ . htmlElemHasName "body"

testName :: Test
testName = testCase "query name" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.name "body" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.name "x" >> Q.succ True

testAttr :: Test
testAttr = testCase "query attr" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.attr "id" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.attr "x" >> Q.succ True

testAttrVal :: Test
testAttrVal = testCase "query attr val" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.attrVal "id" "x" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.attrVal "id" "0" >> Q.succ True

testId :: Test
testId = testCase "query id" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.id "x" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.id "0" >> Q.succ True

testClass :: Test
testClass = testCase "query class" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.hasClass "z" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run b $ Q.last >> Q.first >> Q.next >> Q.hasClass "0" >> Q.succ True

testGeneral :: Test
testGeneral = testCase "query general" $ do
  assertEqual "TEST 4" (Just "h1") $
    Q.run b $ do
      Q.name "body"
      Q.first
      Q.name "h1"
      Q.save 1
      Q.next
      Q.name "p"
      Q.isLast
      a <- Q.get 1
      Q.succ $ htmlElemName a

testOnly :: Test
testOnly = testCase "query only" $ do
  assertEqual "TEST 1" (Just True) $
    Q.run a $ Q.first >> Q.next >> Q.only "span" >> Q.only "a" >> Q.succ True
  assertEqual "TEST 2" (Nothing) $
    Q.run a $ Q.first >> Q.next >> Q.only "a" >> Q.only "a" >> Q.succ True
  assertEqual "TEST 3" (Nothing) $
    Q.run a $ Q.only "h1" >> Q.succ True
  where
    a = fromJust $ htmlDocBody $ htmlParseEasy
      "<h1></h1><p><span><a></a></span></p>"
