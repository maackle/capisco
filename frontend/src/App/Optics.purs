module App.Optics where

import Prelude

import Data.Maybe (Maybe)
import Data.Lens
import Data.Lens.At
import Data.List (foldl)

import App.State


articleLinks :: Lens' Article (Maybe SlugMap)
articleLinks =
  lens get set
  where
    get :: Article -> (Maybe SlugMap)
    get (Article a) = a.links

    set :: Article -> (Maybe SlugMap) -> Article
    set (Article a) m = Article $ a { links = m }

lArticle slug = articleLinks <<< _Just <<< at slug

mkArticleLens slugpath =
  (foldl comp id $ slugpath)
  where
    comp ln slug =
       ln <<< (lArticle slug) <<< _Just
