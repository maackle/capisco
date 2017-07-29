module App.Events where

import App.State
import Data.Either
import Prelude

import App.Optics (mkArticleLens, rootArticle)
import App.Parsers (parseSubtree)
import App.Routes (Route)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Bifunctor (bimap, lmap)
import Data.Lens (_Just, (%~), (.~), (^?))
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, get)
import Pux (EffModel, CoreEffects, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Util (normalizeURL)

data Event
  = PageView Route
  | ChangeInput DOMEvent
  | InitRootArticle DOMEvent
  | SetArticleToggle SlugPath Boolean

  | RequestMarkArticle SlugPath Known

  | RequestArticleTree SlugPath
  | ReceiveArticleTree SlugPath (Either String (Array Article))

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

nofx = noEffects

withfx = { state: _, effects: _ }

fxLog msg = [ log msg *> pure Nothing ]
fxError msg = [ error msg *> pure Nothing ]

type Model s fx = EffModel s Event (AppEffects fx)
type FX fx = Aff (AppEffects (CoreEffects fx)) (Maybe Event)

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

foldp (SetArticleToggle slugpath expanded) state =
  updateArticleW slugpath state \(Article a) ->
    pure $ Article a {expanded = expanded}

foldp (RequestMarkArticle slugpath known) state =
  updateArticleW slugpath state \(Article a) -> do
    tell [ pure $ Just $ RequestArticleTree slugpath ]
    pure $ Article a { known = known }

foldp (RequestArticleTree slugpath) state =
  case getArticle slugpath state of
    Just (Article article) ->
      let
        fetchEffect = do
          let url = normalizeURL $ state.config.apiBase <> "/lookup/" <> article.slug
          res <- attempt $ get url

          let
            -- Just to see what's going on here
            r :: Either Error (AffjaxResponse String)
            r = res

            result :: Either String String
            result = lmap show $ r <#> _.response

          pure $ Just $ ReceiveArticleTree slugpath (result >>= parseSubtree)

        expandEffect = pure $ Just $ SetArticleToggle slugpath true

      in withfx state $ [ fetchEffect, expandEffect ]
    Nothing ->
      nofx state

foldp (ReceiveArticleTree slugpath result) state =
  case result of
    Left err -> withfx state $ fxError err
    Right articles ->
      updateArticleW slugpath state \(Article a) ->
        pure $ Article a
          { links = Just $ mkSlugMap articles
          , expanded = true }


updateRoute :: Route -> RoutingState -> RoutingState
updateRoute route state = state { route = route, loaded = true }

getArticle :: SlugPath -> State -> Maybe Article
getArticle slugpath s =
  s ^? (mkArticleLens slugpath) <<< _Just

updateArticle :: SlugPath -> State -> (ArticleData -> ArticleData) -> (Maybe State)
updateArticle slugpath s update =
  getArticle slugpath s <#> \_ ->
    s # mkArticleLens slugpath <<< _Just %~ \(Article a) -> Article $ update a

-- Update Article at slugpath using a monadic function over Writer
-- for generating effects while doing the update
updateArticleW :: ∀ fx. SlugPath -> State -> (Article -> Writer (Array (FX fx)) Article) -> Model State fx
updateArticleW slugpath state update =
  case getArticle slugpath state of
    Just article ->
      let
        Tuple article' effects = runWriter $ update article
        ln = mkArticleLens slugpath <<< _Just
      in
        { state: state # ln .~ article'
        , effects: effects
        }
    Nothing ->
      { state: state
      , effects: [ error "Could not find article!" *> pure Nothing ]
      }

removeArticle :: SlugPath -> State -> State
removeArticle slugpath s =
  s # mkArticleLens slugpath .~ Nothing

setRootArticle :: Article -> State -> State
setRootArticle article state =
  state # rootArticle .~ Just article
