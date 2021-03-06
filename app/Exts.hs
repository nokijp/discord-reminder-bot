module Exts
  ( throwE
  , maybeT
  , exitM
  , toText
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import qualified Data.Text as T

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return

exitM :: Monad m => MaybeT m a
exitM = maybeT Nothing


toText :: Show a => a -> Text
toText = T.pack . show
