module App.View.Homepage where

import Data.Maybe
import Text.Smolder.HTML

import App.Events (Event(..))
import App.State (Article(..), State(..), SlugPath)
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (flip, ($))
import Data.List (List(..), snoc)
import Prelude hiding (div)
import Pux.DOM.Events (onChange, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML.Attributes (className, href, type', value)
import Text.Smolder.Markup (text, (!), (#!))


type View = State -> HTML Event

view :: View
view (State st) =
  div do
    h1 $ text "¿capisce?"
    input ! type' "text" ! value st.inputText #! onChange ChangeInput
    button #! onClick InitRootArticle $ text "Set"
    div $ for_ st.article $ flip viewArticleTree Nil

viewArticleTree :: Article -> SlugPath -> HTML Event
viewArticleTree (Article a) slugpath =
  ul subtree
  where
    subtree =
      if a.expanded
      then
        case a.links of
          Nothing ->
            nameDisplay
          Just links -> do
            nameDisplay
            for_ links \(Article a) ->
              li $ text a.slug
      else
        nameDisplay

    nameDisplay =
      div do
        h2 $ text $ a.slug <> "!" -- TODO: recursive call with (snoc slugpath a.slug)
        button #! onClick (ToggleArticle slugpath) $ text "toggle"
