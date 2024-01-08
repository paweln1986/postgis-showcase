module AppM where

import App.Navigate (class Navigate)
import App.Route (routeCodec)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (unsafeToForeign)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), bind, ($))
import Routing.Duplex (print)
import Routing.PushState (PushStateInterface)
import Type.Equality (class TypeEquals, from)

type Env
  = { navInterface :: PushStateInterface
    }

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadThrowAppM :: MonadThrow Error AppM

instance monadAskAppM :: (TypeEquals e Env) => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate route = do
    aa <- asks _.navInterface
    let
      a = print routeCodec route
    liftEffect $ aa.pushState (unsafeToForeign "") a
