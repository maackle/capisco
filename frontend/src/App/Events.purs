module App.Events where

import App.State
import Data.Either
import Prelude

import App.Optics (mkArticleLens, rootArticle)
import App.Parsers (parseSubtree)
import App.Routes (Route)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (Error)
import Data.Bifunctor (bimap, lmap)
import Data.Lens (_Just, (%~), (.~), (^?))
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Util (normalizeURL)

data Event
  = PageView Route
  | ChangeInput DOMEvent
  | InitRootArticle DOMEvent
  | ToggleArticle SlugPath DOMEvent

  | FetchArticleTree SlugPath
  | ReceiveArticleTree SlugPath (Either String (Array Article))

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

nofx = noEffects

withfx = { state: _, effects: _ }

fxLog msg = [ log msg *> pure Nothing ]
fxError msg = [ error msg *> pure Nothing ]

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)

foldp (PageView route) (State st) =
  nofx $ State st { routing = updateRoute route st.routing }

foldp (ChangeInput ev) (State st) =
  nofx $ (State st { inputText = targetValue ev })

foldp (InitRootArticle ev) state@(State st) =
  withfx
  (setRootArticle article state)
  $ [ pure $ Just $ FetchArticleTree Nil ]
    where
      slug = st.inputText
      article = initArticle slug

foldp (ToggleArticle slugpath _) state@(State st) =
  nofx $ case updateArticle slugpath state (\a -> a {expanded = not a.expanded}) of
    Just state' -> state'
    Nothing -> state -- TODO: same thing, write function to map over Maybe and produce error effects

foldp (FetchArticleTree slugpath) state@(State st) =
  case getArticle slugpath state of
    Just (Article article) ->
      withfx state $ [ do
        let url = normalizeURL $ st.config.apiBase <> "/lookup/" <> article.slug
        res <- attempt $ get url

        let
          -- Just to see what's going on here
          r :: Either Error (AffjaxResponse String)
          r = res

          result :: Either String String
          result = lmap show $ r <#> _.response

        pure $ Just $ ReceiveArticleTree slugpath (result >>= parseSubtree)
      ]
    Nothing ->
      nofx state

foldp (ReceiveArticleTree slugpath result) state@(State st) =
  case result of
    Left err -> withfx state $ fxError err
    Right articles ->
      nofx $ case updateArticle slugpath state (_ { links = Just $ mkSlugMap articles }) of
        Just state' -> state'
        Nothing -> state -- TODO: same thing, write function to map over Maybe and produce error effects



updateRoute :: Route -> RoutingState -> RoutingState
updateRoute route st = st { route = route, loaded = true }

getArticle :: SlugPath -> State -> Maybe Article
getArticle slugpath s =
  s ^? (mkArticleLens slugpath) <<< _Just

updateArticle :: SlugPath -> State -> (ArticleData -> ArticleData) -> (Maybe State)
updateArticle slugpath s update =
  getArticle slugpath s <#> \_ ->
    s # mkArticleLens slugpath <<< _Just %~ \(Article a) -> Article $ update a

removeArticle :: SlugPath -> State -> State
removeArticle slugpath s =
  s # mkArticleLens slugpath .~ Nothing

setRootArticle :: Article -> State -> State
setRootArticle article state =
  state # rootArticle .~ Just article
