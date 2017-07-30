module App.Parsers where

import Data.Maybe
import Prelude

import App.State (Article(..), Known(..), initArticle)
import Control.Monad.Except (catchError, runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.String.Read (read)
import Data.Traversable (traverse)
import Util.Foreign (foreignValue)

readSubtree :: Foreign -> F (Array Article)
readSubtree json =
  traverse readArticle =<< readArray json
  where
    readArticle :: Foreign -> F Article
    readArticle a = do
      slug <- a ! "slug" >>= readString
      knownStr <- a ! "known" >>= readString
      known <- maybe
                  (fail $ ForeignError "bad 'known' value")
                  pure
                  (read knownStr :: Maybe Known)
      pure $ initArticle slug known

parseSubtree :: String -> Either String (Array Article)
parseSubtree json =
  (lmap catch) $ runExcept $ readSubtree =<< foreignValue json
  where
    catch es = show es
