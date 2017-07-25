module App.View.Homepage where

import Data.Maybe
import Text.Smolder.HTML

import App.Events (Event(..))
import App.State (Article(..), State(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (($))
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
    div $ for_ st.article viewArticleTree

viewArticleTree :: Article -> HTML Event
viewArticleTree (Article a) =
  div do
    h2 $ text $ a.slug <> "!"
    subtree
  where
    subtree = case a.links of
      Nothing -> text "no subtree"
      Just links -> ul $ for_ links \(Article a) ->
        li $ text a.slug
