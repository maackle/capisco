module Util where

import Data.Maybe
import Prelude

import Data.Either (fromRight)
import Data.String (Pattern(..), indexOf, splitAt)
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (global)
import Partial.Unsafe (unsafePartial)

normalizePath :: String -> String
normalizePath path =
  replace pattern "/" path
  where
    pattern :: Regex
    pattern = unsafePartial $ fromRight $ regex "/+" global

normalizeURL :: String -> String
normalizeURL url =
  case indexOf (Pattern "://") url of
    Nothing -> normalizePath url
    Just i ->
      case splitAt (i + 3) url of
        Nothing -> normalizePath url
        Just {before, after} ->
          before <> normalizePath after
