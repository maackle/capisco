module App.State where

import App.Config (config)
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
  , routing:
    { title: config.title
      , route: match url
      , loaded: false
    }
  }

newtype State = State
  { routing :: RoutingState
  , article :: Maybe Article
  }

newtype Article = Article
  { slug :: Slug
  , url :: String
  , expanded :: Boolean
  , known :: Known
  , links :: Maybe (Map Slug Article)
  }

instance showArticle :: Show Article where
  show (Article a) = "Article<" <> a.slug <> "> " <> show a.links

type Slug = String
type SlugPath = List Slug

data Known = KnownVoid | KnownNo | KnownYes

type RoutingState =
  { title :: String
  , route :: Route
  , loaded :: Boolean
  }

derive instance newtypeState :: Newtype State _
derive instance newtypeArticle :: Newtype Article _
