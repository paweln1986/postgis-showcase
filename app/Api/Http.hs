{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Api.Http (serveApi) where

import Colog (Message, Severity (Error))
import Colog.Actions (richMessageAction)
import Data.OpenApi.Lens (
  HasDescription (description),
  HasInfo (info),
  HasTitle (title),
  HasVersion (version),
 )
import Database.Connection (DatabaseConnection, DatabaseError, handleDatabaseConnection)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runErrorWith)
import Effectful.Reader.Static (Reader, runReader)
import Hasql.Connection qualified as Connection
import Hasql.Pool (Pool, acquire, release)
import Lens.Micro ((.~), (?~))
import Network.Wai.Handler.Warp (run)
import Scope.Http.Api (ScopeApi, scopeApiHandler)
import Scope.Registry (ScopeRegistry, handleScopeRegistry)
import Servant (Handler, HasServer (ServerT), Server, ServerError (errBody), hoistServer, serve, type (:<|>) (..), type (:>))
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server (err500)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import UnliftIO (throwIO)
import UnliftIO.Resource (allocate, runResourceT)
import Utils.Log (Log, logWithCallStack, runLog)
import Prelude hiding (Reader, runReader)

type HttpApi = "api" :> ScopeApi

handler :: ServerT HttpApi (Eff (ScopeRegistry : Error ServerError : es))
handler = scopeApiHandler

hoistedHandler :: Pool -> Server HttpApi
hoistedHandler pool = hoistServer apiProxy nt handler
 where
  apiProxy = Proxy :: Proxy HttpApi
  nt :: Eff '[ScopeRegistry, Error ServerError, DatabaseConnection, Reader Pool, Error DatabaseError, Log Message, IOE] x -> Handler x
  nt x =
    liftIO
      $ runEff
      . runLog richMessageAction
      . runErrorWith @DatabaseError handleDatabaseError
      . runReader pool
      . handleDatabaseConnection
      . runErrorWith @ServerError handleServerError
      . handleScopeRegistry
      $ x
  handleDatabaseError stack err = do
    logWithCallStack Error stack (show err)
    throwIO $ err500{errBody = show err}
  handleServerError stack err = do
    logWithCallStack Error stack (show err)
    throwIO err

type ApiWitDoc = HttpApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

apiWithDocProxy :: Proxy ApiWitDoc
apiWithDocProxy = Proxy :: Proxy ApiWitDoc

appHandler :: Pool -> Server ApiWitDoc
appHandler pool = hoistedHandler pool :<|> swaggerSchemaUIServer swaggerDoc
 where
  swaggerDoc =
    toOpenApi (Proxy :: Proxy HttpApi)
      & info
      . title
      .~ "Scope registry API"
      & info
      . version
      .~ "2023.12.26"
      & info
      . description
      ?~ "This is an API manage scopes and antenna data"

serveApi :: IO ()
serveApi = runResourceT $ do
  postgisPool <- acquireDatabasePool
  liftIO $ run 8000 (serve apiWithDocProxy (appHandler postgisPool))
 where
  acquireDatabasePool = do
    snd <$> allocate (createPostgresqlPool connectionSettings) release
  connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "test"
  createPostgresqlPool = acquire 3 10 1_800 1_800