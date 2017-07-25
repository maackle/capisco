module App.Events where

import Prelude

import App.Optics (mkArticleLens)
import App.Routes (Route)
import App.State (Article(..), RoutingState, SlugPath, State(State))
import Data.Lens (_Just, (%~), (.~), (^?))
import Data.List (unsnoc)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event
  = PageView Route
  | ToggleArticle SlugPath


type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)

foldp (PageView route) (State st) =
  noEffects $ State st { routing = updateRoute route st.routing }

foldp (ToggleArticle slugpath) (State st) =
  noEffects $ State st -- .article

updateRoute :: Route -> RoutingState -> RoutingState
updateRoute route st = st { route = route, loaded = true }

getArticle :: SlugPath -> State -> Maybe Article
getArticle slugpath (State st) =
  st.article ^? _Just <<< (mkArticleLens slugpath)

updateArticle :: SlugPath -> (Article -> Article) -> State -> State
updateArticle slugpath update (State st) =
  State st { article = article' }
  where
    article' =
      case st.article of
        Nothing -> st.article
        (Just a) ->
          Just $ a # (mkArticleLens slugpath) %~ update

removeArticle :: SlugPath -> State -> State
removeArticle slugpath (State st) =
  st.article # _Just <<< mkArticleLens slugpath .~ Nothing
