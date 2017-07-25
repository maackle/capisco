module Util.Foreign where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail)
import Data.Function.Uncurried (Fn3, runFn3)

foreign import foreignValueImpl :: forall r. Fn3 (String -> r) (Foreign -> r) String r

foreignValue :: String -> F Foreign
foreignValue json = runFn3 foreignValueImpl (fail <<< JSONError) pure json
