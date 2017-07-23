module App.Events where

import App.Routes (Route)
import App.State (State(..), RoutingState)
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event = PageView Route

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { routing = updateRoute route st.routing }

updateRoute :: Route -> RoutingState -> RoutingState
updateRoute route st = st { route = route, loaded = true }
