{-# LANGUAGE TemplateHaskell #-}

module Database.Connection where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Hasql.Pool (Pool, use)
import Hasql.Session (Session)
import Prelude hiding (Reader, ask)

newtype DatabaseError = DatabaseError
    { message :: Text
    }
    deriving stock (Show)

instance Exception DatabaseError

data DatabaseConnection :: Effect where
    RunStatement :: Session a -> DatabaseConnection m a

makeEffect ''DatabaseConnection

handleDatabaseConnection :: (Reader Pool :> es, IOE :> es, Error DatabaseError :> es) => Eff (DatabaseConnection : es) a -> Eff es a
handleDatabaseConnection = interpret $ \_ (RunStatement session) -> do
    pool <- ask
    res <- liftIO $ use pool session
    case res of
        Left err -> throwError $ DatabaseError $ show err
        Right success -> pure success