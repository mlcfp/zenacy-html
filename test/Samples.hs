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
