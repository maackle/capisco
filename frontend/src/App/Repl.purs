module App.Repl
  ( module Prelude

  , module Data.Lens
  , module Data.Lens.At
  , module Data.Map
  , module Data.Maybe
  , module Data.Tuple

  , module App.Events
  , module App.Optics
  , module App.State

  , quux
  , foo
  , bar
  , testMap
  , mkPath

  ) where

import Prelude

import Data.Lens
import Data.Lens.At
import Data.List as L
import Data.Map
import Data.Maybe
import Data.Tuple hiding (lookup)

import App.Events
import App.Optics
import App.State


quux :: Article
quux =
  Article
    { slug: "quux"
    , url: "/wiki/quux"
    , expanded: false
    , known: KnownVoid
    , links: Just $ fromFoldable [Tuple "bar" $ bar]
    }

bar :: Article
bar =
  Article
    { slug: "bar"
    , url: "/wiki/bar"
    , expanded: false
    , known: KnownVoid
    , links: Just $ fromFoldable [Tuple "foo" $ foo]
    }

foo :: Article
foo =
  Article {
    slug: "foo"
    , url: "/wiki/foo"
    , expanded: false
    , known: KnownVoid
    , links: Nothing
  }

testMap :: Map String (Map String (Map String (Map String String)))
testMap =
  fromFoldable [Tuple "a" $
    fromFoldable [Tuple "b" $
      fromFoldable [Tuple "c" empty]
      ]
    ]

mkPath a = mkArticleLens $ L.fromFoldable a

--
-- examples = do
--   let ln = (getArticleLens $ L.fromFoldable ["bar"])
--   let get1 = preview ln quux
--   let get2 = quux ^? ln
--
--   -- TODO: is there a Prism version of set?
--   let set1 = set ln quux foo
--   pure
