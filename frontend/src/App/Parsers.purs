module App.Parsers where

import App.Error
import Data.Maybe
import Debug.Trace
import Prelude

import App.State (Article(..), Known(..), initArticle)
import Control.Monad.Except (catchError, mapExcept, runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, toForeign, typeOf)
import Data.Foreign.Index ((!), ix)
import Data.String.Read (read)
import Data.Traversable (traverse)
import Util.Foreign (foreignValue)

readSubtree :: Foreign -> F (Array Article)
readSubtree json =
  traverse readArticle =<< readArray =<< (json ! "links")
  where
    readArticle :: Foreign -> F Article
    readArticle a = do
      slug <- a ! "slug" >>= readString
      knownStr <- a ! "known" >>= readString
      known <- maybe
                  (fail $ ForeignError "bad 'known' value")
                  pure
                  (read knownStr :: Maybe Known)
      pure $ initArticle slug known Nothing

parseSubtree :: String -> Ex (Array Article)
parseSubtree json =
  mapAppErrors $ (readSubtree =<< foreignValue json)

parsePreview :: String -> Ex String
parsePreview json =
  mapAppErrors $ readString =<< (_ ! "preview") =<< foreignValue json
--
