module App.State where

import App.Config (Config(..), config)
import App.Routes (Route, match)
import Data.List (List)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..))
import Prelude

init :: String -> State
init url = State
  { article: Nothing
  , inputText: ""
  , config: config
  , routing:
    { title: config.title
      , route: match url
      , loaded: false
    }
  }

newtype State = State
  { routing :: RoutingState
  , config :: Config
  , article :: Maybe Article
  , inputText :: String
  }

newtype Article = Article ArticleData
type ArticleData =
  { slug :: Slug
  , url :: String
  , expanded :: Boolean
  , known :: Known
  , links :: Maybe (Map Slug Article)
  }

initArticle :: Slug -> Article
initArticle slug = Article
  { slug: slug
  , url: "https://en.wikipedia.org/wiki/" <> slug
  , expanded: false
  , known: KnownVoid
  , links: Nothing
  }

instance showArticle :: Show Article where
  show (Article a) = "Article<" <> a.slug <> "> " <> show a.links

type Slug = String
type SlugPath = List Slug
type SlugMap = Map Slug Article

mkSlugMap :: (Array Article) -> SlugMap
mkSlugMap articles =
  M.fromFoldable
  $ articles
    <#> \article@(Article a) -> Tuple a.slug article

data Known = KnownVoid | KnownNo | KnownYes

type RoutingState =
  { title :: String
  , route :: Route
  , loaded :: Boolean
  }

derive instance newtypeState :: Newtype State _
derive instance newtypeArticle :: Newtype Article _
