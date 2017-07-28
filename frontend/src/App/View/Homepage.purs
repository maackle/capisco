module App.View.Homepage where

import Data.Maybe
import Text.Smolder.HTML

import App.Events (Event(..))
import App.State (Article(..), Known(..), SlugPath, State(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (flip, ($))
import Data.List (List(..), length, snoc)
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
viewArticleTree article@(Article a) slugpath =
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
            for_ links \article'@(Article a') ->
              li $ viewArticleTree article' (snoc slugpath a'.slug)
      else
        nameDisplay

    -- at least one layer deep
    isSubArticle = length slugpath > 0

    nameDisplay =
      div do
        h2 $ text $ a.slug
        button #! onClick (const $ RequestMarkArticle slugpath KnownNo) $ text "mark not known"
        button #! onClick (ToggleArticle slugpath) $ text buttonText
      where
        buttonText = if a.expanded then "collapse" else "expand"
