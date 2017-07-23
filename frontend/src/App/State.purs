module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Newtype (class Newtype)

newtype State = State
  { routing :: RoutingState
  }

type RoutingState =
  { title :: String
  , route :: Route
  , loaded :: Boolean
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { routing:
    { title: config.title
      , route: match url
      , loaded: false
    }
  }
