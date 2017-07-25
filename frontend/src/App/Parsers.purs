module App.Parsers where

import Prelude

import App.State (Article(..), initArticle)
import Control.Monad.Except (catchError, runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foreign (Foreign, F, readArray, readString)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)
import Util.Foreign (foreignValue)

readSubtree :: Foreign -> F (Array Article)
readSubtree json =
  traverse readArticle =<< readArray json
  where
    readArticle :: Foreign -> F Article
    readArticle a = do
      slug <- a ! "slug" >>= readString
      pure $ initArticle slug

parseSubtree :: String -> Either String (Array Article)
parseSubtree json =
  (lmap catch) $ runExcept $ readSubtree =<< foreignValue json
  where
    catch es = show es
