--------------------------------------------------------------------------------
-- Copyright (C) 2016 Zenacy Reader Technologies LLC
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Samples
  ( testSamples
  ) where

import Zenacy.HTML
import Data.Default
  ( Default(..)
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
import Data.Maybe
  ( fromJust
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( pack
  )
import Text.RawString.QQ

testSamples :: Test
testSamples = testGroup "Samples"
  [ testHello
  , testRewrite
  , testExtract
  ]

testHello :: Test
testHello = testCase "sample hello" $ do
  flip (assertEqual "Sample 1")
    (htmlRender $ htmlParseEasy "<div>HelloWorld</div>")
    "<html><head></head><body><div>HelloWorld</div></body></html>"

  flip (assertEqual "Sample 2")
    (htmlParseEasy "<div>HelloWorld</div>")
    (HTMLDocument ""
      [ HTMLElement "html" HTMLNamespaceHTML []
          [ HTMLElement "head" HTMLNamespaceHTML [] []
          , HTMLElement "body" HTMLNamespaceHTML []
            [ HTMLElement "div" HTMLNamespaceHTML []
              [ HTMLText "HelloWorld" ] ] ] ])

  flip (assertEqual "Sample 3")
    (htmlParseEasy "<div>HelloWorld</div>")
    HTMLDocument
      { htmlDocumentName = ""
      , htmlDocumentChildren =
        [ HTMLElement
          { htmlElementName = "html"
          , htmlElementNamespace = HTMLNamespaceHTML
          , htmlElementAttributes = []
          , htmlElementChildren =
            [ HTMLElement
              { htmlElementName = "head"
              , htmlElementNamespace = HTMLNamespaceHTML
              , htmlElementAttributes = []
              , htmlElementChildren = []
              }
            , HTMLElement
              { htmlElementName = "body"
              , htmlElementNamespace = HTMLNamespaceHTML
              , htmlElementAttributes = []
              , htmlElementChildren =
                [ HTMLElement
                  { htmlElementName = "div"
                  , htmlElementNamespace = HTMLNamespaceHTML
                  , htmlElementAttributes = []
                  , htmlElementChildren =
                    [ HTMLText
                      { htmlTextData = "HelloWorld" }
                    ] } ] } ] } ] }

testRewrite :: Test
testRewrite = testCase "sample rewrite" $ do
  flip (assertEqual "Sample 1")
    (rewrite "<span>Hello</span><span>World</span>")
    "<html><head></head><body><div>Hello</div><div>World</div></body></html>"

rewrite :: Text -> Text
rewrite = htmlRender . htmlMapElem f . fromJust . htmlDocHtml . htmlParseEasy
  where
    f x
      | htmlElemHasName "span" x = htmlElemRename "div" x
      | otherwise = x

testExtract :: Test
testExtract = testCase "sample extract" $ do
  flip (assertEqual "Sample 1")
    (extract "<a href=\"https://example1.com\"></a><a href=\"https://example2.com\"></a>")
    [ "https://example1.com"
    , "https://example2.com"
    ]

extract :: Text -> [Text]
extract = go . htmlParseEasy
  where
    go = \case
      HTMLDocument n c ->
        concatMap go c
      e @ (HTMLElement "a" s a c) ->
        case htmlElemAttrFind (htmlAttrHasName "href") e of
          Just (HTMLAttr n v s) ->
            v : concatMap go c
          Nothing ->
            concatMap go c
      HTMLElement n s a c ->
        concatMap go c
      _otherwise ->
        []
