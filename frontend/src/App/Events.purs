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

  | RequestMarkArticle SlugPath Known

  | RequestArticleTree SlugPath
  | ReceiveArticleTree SlugPath (Either String (Array Article))

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

nofx = noEffects

withfx = { state: _, effects: _ }

fxLog msg = [ log msg *> pure Nothing ]
fxError msg = [ error msg *> pure Nothing ]

type Model s fx = EffModel s Event (AppEffects fx)

foldp :: ∀ fx. Event -> State -> Model State fx

foldp (PageView route) state =
  nofx $ state { routing = updateRoute route state.routing }

foldp (ChangeInput ev) state =
  nofx $ (state { inputText = targetValue ev })

foldp (InitRootArticle ev) state =
  withfx
  (setRootArticle article state)
  $ [ pure $ Just $ RequestArticleTree Nil ]
    where
      slug = state.inputText
      article = initArticle slug

foldp (ToggleArticle slugpath _) state =

  case updateArticle slugpath state (\a -> a {expanded = not a.expanded}) of
    Just state' ->
      { state: state'
      , effects: [ --pure =<< state'.article]
      ]
      }
    Nothing -> nofx state -- TODO: same thing, write function to map over Maybe and produce error effects

foldp (RequestMarkArticle slugpath known) state =
  case updateArticle slugpath state (_ { known = known }) of
    Nothing ->
      { state: state
      , effects: []
      }
    Just state' ->
      { state: state'
      , effects: [ pure $ Just $ RequestArticleTree slugpath ]
      }

foldp (RequestArticleTree slugpath) state =
  case getArticle slugpath state of
    Just (Article article) ->
      -- TODO: set article expanded here
      withfx state $ [ do
        let url = normalizeURL $ state.config.apiBase <> "/lookup/" <> article.slug
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

foldp (ReceiveArticleTree slugpath result) state =
  case result of
    Left err -> withfx state $ fxError err
    Right articles ->
      nofx $ case updateArticle slugpath state (_ { links = Just $ mkSlugMap articles, expanded = true }) of
        Just state' -> state'
        Nothing -> state -- TODO: same thing, write function to map over Maybe and produce error effects



updateRoute :: Route -> RoutingState -> RoutingState
updateRoute route state = state { route = route, loaded = true }

getArticle :: SlugPath -> State -> Maybe Article
getArticle slugpath s =
  s ^? (mkArticleLens slugpath) <<< _Just

updateArticle :: SlugPath -> State -> (ArticleData -> ArticleData) -> (Maybe State)
updateArticle slugpath s update =
  getArticle slugpath s <#> \_ ->
    s # mkArticleLens slugpath <<< _Just %~ \(Article a) -> Article $ update a
--
-- -- TODO: find a way to update Article AND effects, either sequentially or through a Traversal
-- updateArticle' :: SlugPath -> State -> (Article -> Model Article fx) -> Model State fx
-- updateArticle' slugpath state update =
--   case getArticle slugpath state of
--     Just _ -> -- have to get the article again with %~ so we just throw it away...
--       state # mkArticleLens slugpath <<< _Just %~
--         update >>> \(Tuple a' fx) ->
--           { article: Article a'
--           , effects: fx }
--     Nothing ->
--       { state: state.article}

removeArticle :: SlugPath -> State -> State
removeArticle slugpath s =
  s # mkArticleLens slugpath .~ Nothing

setRootArticle :: Article -> State -> State
setRootArticle article state =
  state # rootArticle .~ Just article
