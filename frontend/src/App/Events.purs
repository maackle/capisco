module App.Events where

import Prelude

import App.Routes (Route)
import App.State (Article(..), RoutingState, SlugPath, State(..), Slug)
import Data.Lens (Lens', _Just, lens)
import Data.Lens.At (at)
import Data.List (foldl)
import Data.Map as M
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
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

-- getArticle :: SlugPath -> Article -> Maybe Article
-- getArticle slugpath article =
--   foldl' look (Just article) slugpath
--   where
--     look :: Article -> String -> Maybe Article
--     look v k =
--       case v.links of
--         Nothing -> Nothing
--         Just(links) -> M.lookup k v.links

-- updateArticle :: SlugPath -> (Article -> Article) -> Article -> Article
-- updateArticle slugpath update article =
--   foldl' look st.article slugpath
--   where
--     look v k = M.lookup k v

-- the one that took all day :(((
--
-- getInArticle ::
--   ∀ f. Functor f =>
--   Slug -> (Maybe Article -> f (Maybe Article)) -> Article -> f Article
-- getInArticle :: Slug -> ArticleLens
-- getInArticle slug lensFn (Article a) =
--   case a.links of
--     Nothing -> fromMaybe (Article a) <$> (lensFn $ Just (Article a))
--     Just links ->
--       let
--         child :: Maybe Article
--         child = M.lookup slug links
--
--         wrap :: Maybe Article -> Article
--         wrap Nothing =
--           Article a
--         wrap (Just article') =
--           Article $ a { links = Just (M.insert slug article' links) }
--       in
--         wrap <$> lensFn child

type SlugMap = (M.Map Slug Article)

articleLinks :: Lens' Article (Maybe SlugMap)
articleLinks =
  lens get set
  where
    get :: Article -> (Maybe SlugMap)
    get (Article a) = a.links

    set :: Article -> (Maybe SlugMap) -> Article
    set (Article a) m = Article $ a { links = m }

-- lArticle :: Slug -> Lens' Article (Maybe Article)
lArticle slug = articleLinks <<< _Just <<< at slug

type ArticleLens = forall f. Strong f => Choice f => String -> f (Maybe Article) (Maybe Article) -> f Article Article
--
-- _Just :: forall a b. Prism (Maybe a) (Maybe b) a b
-- _Just = prism Just $ maybe (Left Nothing) Right


-- getArticleLens :: SlugPath -> ArticleLens
getArticleLens slugpath =
  (foldl comp j $ slugpath)
  where
    j = id --lens Just const
    -- comp :: ArticleLens -> Slug -> ArticleLens
    comp ln slug =
       ln <<< (lArticle slug) <<< _Just
--


--
-- getArticle :: SlugPath -> Article -> Maybe Article
-- getArticle slugpath article =
--   foldl' look article (toUnfoldable slugpath)
--   where
--     look :: Article -> Slug -> Maybe Article
--     look (Article article) slug =
--       article.links >>= (\links -> M.lookup slug links)

-- from https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation


--
-- at ::
--    ∀ k v. Ord k =>
--    k -> Lens' (M.Map k v) (Maybe v)
-- at k lensFn m =
--   wrap <$> (lensFn mv)
--   where
--     mv :: Maybe v
--     mv = M.lookup k m
--
--     wrap :: Maybe v -> (M.Map k v)
--     wrap (Just v') = M.insert k v' m
--     wrap Nothing   =
--       case mv of
--         Nothing -> m
--         Just _  -> M.delete k m
