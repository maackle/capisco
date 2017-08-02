module App.Events where

import App.Error
import App.State
import Data.Either
import Prelude

import App.Optics (mkArticleLens, rootArticle)
import App.Parsers (parseSubtree, parsePreview)
import App.Routes (Route)
import Control.Monad.Aff (Aff, attempt, liftEff')
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (except, mapExcept, runExcept)
import Control.Monad.Writer (Writer, execWriter, runWriter, tell)
import Data.Array (concat, foldl, fromFoldable, uncons)
import Data.Bifunctor (bimap, lmap)
import Data.Foldable (foldr)
import Data.Lens (_Just, (%~), (.~), (^?))
import Data.List (List(..), (:))
import Data.List.NonEmpty (cons, singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, URL)
import Network.HTTP.Affjax as AX
import Pux (EffModel, CoreEffects, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Util (normalizeURL)

data Event
  = PageView Route
  | ChangeInput DOMEvent
  | InitRootArticle DOMEvent
  | SetArticleToggle SlugPath Boolean

  | ExpandAllRelevant SlugPath
  | CollapseAllRelevant SlugPath

  | RequestMarkArticle SlugPath Known
  | ReceiveMarkArticle SlugPath (Either String Known)

  | RequestArticleData SlugPath
  | ReceiveArticleData SlugPath (Array Article) String

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)

nofx = noEffects

withfx = { state: _, effects: _ }

fxLog msg = [ log msg *> pure Nothing ]
fxError msg = [ error msg *> pure Nothing ]

--
-- notifyException :: ∀ a fx. (Model State fx -> Model State fx) -> Either String a -> Model State fx
-- notifyException f =
--   case _ of
--     Right s -> { state: f s, effects: [] }
--     Left e -> { state: s, effects: [fxError e]}

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
  $ [ pure $ Just $ RequestArticleData Nil ]
    where
      slug = state.inputText
      article = initArticle slug KnownVoid Nothing -- TODO: read known value

foldp (SetArticleToggle slugpath expanded) state =
  updateArticleW slugpath state \(Article a) -> do
    tell $ case expanded, a.links of
      true, Nothing -> [ pure $ Just $ RequestArticleData slugpath ]
      _, _ -> []
    pure $ Article a {expanded = expanded}

-- TODO
foldp (ExpandAllRelevant slugpath) state =
  updateArticleW slugpath state \(Article a) ->
    pure $ (Article a { links = a.links <#> (map doExpand) })
    where
      doExpand :: Article -> Article
      doExpand (Article a) =
        case a.known, a.links of
          KnownNo, Just links ->
            Article a { links = Just $ links <#> doExpand, expanded = true }
          _, _ -> Article a

-- TODO
foldp (CollapseAllRelevant slugpath) state =
  updateArticleW slugpath state \(Article a) ->
    pure $ (Article a { links = a.links <#> (map doCollapse) })
    where
      doCollapse :: Article -> Article
      doCollapse (Article a) =
        case a.known, a.links of
          KnownNo, Just links ->
            Article a { links = Just $ links <#> doCollapse, expanded = false }
          _, _ -> Article a
-- -- TODO
-- foldp (ExpandAllRelevant slugpath) state =
--   updateArticleW slugpath state \(Article a) ->
--     doExpand (Article a)
--     where
--       doExpand :: Article -> Writer (Array (FX fx)) Article
--       doExpand (Article a) =
--         case a.known, a.links of
--           KnownNo, Just links ->
--             let
--               writers :: M.Map Slug (Writer (Array (FX fx)) Article)
--               writers = links <#> doExpand
--
--               links' = writers <#> (fst <<< runWriter)
--               -- fx :: FX fx
--               fx = execWriter (sequence $ (fromFoldable $ M.values writers))
--               -- fx = foldl (\w a -> concat $ [a, (execWriter w)]) [] writers
--             in do
--               tell [ pure $ Just $ SetArticleToggle slugpath true]
--               tell fx
--               pure $ Article a {links = Just links'}
--           _, _ -> pure $ Article a


foldp (RequestMarkArticle slugpath known) state =
  case getArticle slugpath state of
    Just (Article article) ->
      let
        url :: URL
        url = normalizeURL
              $ state.config.apiBase
              <> "/mark/"
              <> article.slug
              <> "/"
              <> (show known)

        fetchEffect =
          do
            res <- attempt $ AX.get url
            let
              result :: Either String String
              result = lmap show $ res <#> _.response
            pure $ Just $ ReceiveMarkArticle slugpath (result *> Right known)
      in withfx state [ fetchEffect ]
    Nothing ->
      nofx state

-- | TODO: update current tree with markings if valid
foldp (ReceiveMarkArticle slugpath result) state =
  case result of
    Left err -> withfx state $ fxError err
    Right known ->
      updateArticleW slugpath state \(Article a) -> do
        tell $ case known of
          KnownNo -> [ pure $ Just $ RequestArticleData slugpath ]
          _ -> []
        pure $ Article a { known = known }

foldp (RequestArticleData slugpath) state =
  case getArticle slugpath state of
    Just (Article article) ->
      let
        t :: ∀ fx'. Aff (ajax :: AJAX | fx') (Ex (Maybe Event))
        t = do
          let url :: URL
              url = normalizeURL $ state.config.apiBase <> "/lookup/" <> article.slug
          res <- (mapAppError <<< except) <$> (attempt $ AX.get url)

          let
            x :: Ex (Maybe Event)
            x = do
              subtree <- parseSubtree =<< _.response <$> res
              let
                s :: Array Article
                s = subtree
              preview <- parsePreview =<< _.response <$> res

              pure $ Just $ ReceiveArticleData slugpath subtree preview
          pure x

        run :: ∀ fx'. Ex (Maybe Event) -> (FX fx')
        -- run :: Ex (Array (Maybe Event)) -> Array (FX fx)
        run x = case runExcept x of
          Right fx' -> pure fx'
          Left es ->
            -- (fromFoldable es) <#> (show >>> error >>> \t -> t *> pure Nothing )
            foldr (\e a -> (error $ show e) *> a) (pure Nothing) es
            -- (map ((\t -> t *> pure Nothing) <<< error <<< show) (fromFoldable es))

        -- fx :: ∀ fx'. Array (FX fx')
        fx = run =<< (t)
      in
        withfx state $ [ fx ]
    Nothing ->
      nofx state

foldp (ReceiveArticleData slugpath articles preview) state =
  updateArticleW slugpath state \(Article a) ->
    pure $ Article a
      { links = Just $ mkSlugMap articles
      , preview = Just preview
      , expanded = true }


updateRoute :: Route -> RoutingState -> RoutingState
updateRoute route state = state { route = route, loaded = true }

getArticle :: SlugPath -> State -> Maybe Article
getArticle slugpath s =
  s ^? (mkArticleLens slugpath) <<< _Just

updateArticle :: SlugPath -> State -> (Article -> Article) -> (Maybe State)
updateArticle slugpath s update =
  getArticle slugpath s <#> \_ ->
    s # mkArticleLens slugpath <<< _Just %~ update

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
