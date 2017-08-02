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
view state =
  div do
    viewErrors state
    h1 $ text "¿capisce?"
    input ! type' "text" ! value state.inputText #! onChange ChangeInput
    button #! onClick InitRootArticle $ text "Set"
    br
    button #! onClick (const $ (ExpandAllRelevant Nil)) $ text "Expand all"
    button #! onClick (const $ (CollapseAllRelevant Nil)) $ text "Collapse all"
    div $ for_ state.article $ flip viewArticleTree Nil

viewErrors :: View
viewErrors state =
  case state.errors of
    Just errors -> div $ for_ errors $ (text <<< show)
    Nothing -> div $ text ""

viewArticleTree :: Article -> SlugPath -> HTML Event
viewArticleTree article@(Article a) slugpath =
  ul subtree
  where
    subtree =
      if a.expanded
      then
        case a.links of
          Nothing -> do
            nameDisplay
            text "loading..."
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
        span ! className "known-icon-bullet" $ knownIcon a.known
        text $ a.slug
        text $ fromMaybe "..." a.preview
        for_ [KnownVoid, KnownNo, KnownYes] \k ->
          button #! onClick (const $ RequestMarkArticle slugpath k) $ knownIcon k
        button #! onClick (const $ SetArticleToggle slugpath (not a.expanded)) $ text buttonText
      where
        buttonText = if a.expanded then "collapse" else "expand"

knownIcon :: ∀ e. Known -> HTML e
knownIcon = case _ of
  KnownVoid -> span ! className "known-icon known-void" $ text "?"
  KnownYes  -> span ! className "known-icon known-yes"  $ text "✓"
  KnownNo   -> span ! className "known-icon known-no"   $ text "✗"
