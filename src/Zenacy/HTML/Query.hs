--------------------------------------------------------------------------------
-- Copyright (C) 2016 Zenacy Reader Technologies LLC
--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A very basic query-like DSL.
module Zenacy.HTML.Query
  ( HTMLQuery
  , run
  , exec
  , try
  , stop
  , cont
  , succ
  , zipper
  , node
  , first
  , last
  , next
  , prev
  , up
  , test
  , name
  , isFirst
  , isLast
  , save
  , get
  , getZipper
  , src
  , attr
  , attrVal
  , id
  , hasClass
  , only
  ) where

import Prelude hiding
  ( cont
  , first
  , get
  , id
  , last
  , succ
  , try
  )
import Zenacy.HTML.Internal.HTML
import Zenacy.HTML.Internal.Oper
import Zenacy.HTML.Internal.Zip
import Control.Monad.State
  ( MonadState
  , State
  , evalState
  , modify
  , gets
  )
import Control.Monad.Trans.Maybe
  ( MaybeT(..)
  , runMaybeT
  )
import Data.Bool
  ( bool
  )
import Data.IntMap
  ( IntMap
  )
import qualified Data.IntMap as IntMap
  ( fromList
  , lookup
  , insert
  )
import Data.Maybe
  ( fromMaybe
  , isNothing
  )
import Data.Text
  ( Text
  )
-- import qualified Data.Text as T
--   ( concat
--   )

-- | Defines the query state.
type QueryState = (HTMLZipper, IntMap HTMLZipper)

-- | Defines the type for a query.
newtype HTMLQuery a = HTMLQuery { unQuery :: MaybeT (State QueryState) a }
  deriving (Functor, Applicative, Monad, MonadState QueryState)

-- | Runs a query and returns a result.
run :: HTMLNode -> HTMLQuery a -> Maybe a
run x q = evalState (runMaybeT $ unQuery q) (z, IntMap.fromList [(0,z)])
  where
    z = htmlZip x

-- | Same as run with the arguments flipped.
exec :: HTMLQuery a -> HTMLNode -> Maybe a
exec = flip run

-- | Same as run with the arguments flipped.
try :: HTMLQuery HTMLNode -> HTMLNode -> HTMLNode
try q x = fromMaybe x $ run x q

-- | Wraps a value as a query result.
wrap :: Maybe a -> HTMLQuery a
wrap = HTMLQuery . MaybeT . pure

-- | Returns a result that stops the query.
stop :: HTMLQuery a
stop = wrap $ Nothing

-- | Returns a result that continues the query.
cont :: HTMLQuery ()
cont = succ ()

-- | Returns a successful query result.
succ :: a -> HTMLQuery a
succ = wrap . Just

-- | Gets the current query zipper.
zipper :: HTMLQuery HTMLZipper
zipper = gets fst

-- | Gets the current query node.
node :: HTMLQuery HTMLNode
node = htmlZipNode <$> zipper

-- | Performs a query step with a zipper operation.
withZip :: (HTMLZipper -> Maybe HTMLZipper) -> HTMLQuery ()
withZip f = do
  z <- zipper
  case f z of
    Nothing ->
      stop
    Just z' -> do
      modify $ \(_, m) -> (z', m)
      cont

-- | Moves the query to the first child node.
first :: HTMLQuery ()
first = withZip htmlZipFirst

-- | Moves the query to the last child node.
last :: HTMLQuery ()
last = withZip htmlZipLast

-- | Moves the query to the next sibling node.
next :: HTMLQuery ()
next = withZip htmlZipNext

-- | Moves the query to the previous sibling node.
prev :: HTMLQuery ()
prev = withZip htmlZipPrev

-- | Moves the query to the parent node.
up :: HTMLQuery ()
up = withZip htmlZipUp

-- | Evaluates a test result and continues the query if true.
test :: Bool -> HTMLQuery ()
test = bool stop cont

-- | Tests the current element name.
name :: Text -> HTMLQuery ()
name x = do
  n <- node
  test $ htmlElemName n == x

-- | Tests the current node to see if it is the first sibling.
isFirst :: HTMLQuery ()
isFirst = do
  z <- zipper
  test $ isNothing $ htmlZipPrev z

-- | Tests the current node to see if it is the last sibling.
isLast :: HTMLQuery ()
isLast = do
  z <- zipper
  test $ isNothing $ htmlZipNext z

-- | Saves the current query state.
save :: Int -> HTMLQuery ()
save x = do
  modify $ \(z, m) -> (z, IntMap.insert x z m)
  cont

-- | Gets a saved query node.
get :: Int -> HTMLQuery HTMLNode
get x = htmlZipNode <$> getZipper x

-- | Gets a saved query zipper.
getZipper :: Int -> HTMLQuery HTMLZipper
getZipper x = do
  m <- gets snd
  case IntMap.lookup x m of
    Just z -> succ z
    Nothing -> stop

-- | Gets the source input node.
src :: HTMLQuery HTMLNode
src = get 0

-- | Tests if the current node has an attribute.
attr :: Text -> HTMLQuery ()
attr x = node >>= test . htmlElemHasAttrName x

-- | Tests if the current node has an attribute value.
attrVal :: Text -> Text -> HTMLQuery ()
attrVal n v = node >>= test . htmlElemHasAttrVal n v

-- | Tests if the current node has an id.
id :: Text -> HTMLQuery ()
id x = node >>= test . htmlElemHasID x

-- | Tests if the current node has a class.
hasClass :: Text -> HTMLQuery ()
hasClass x = node >>= test . htmlElemClassesContains x

-- | Moves to the child and require that it is the only child.
only :: Text -> HTMLQuery ()
only x = first >> name x >> isLast
