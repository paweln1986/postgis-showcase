{-# LANGUAGE BlockArguments #-}

module Utils.Log where

import Colog (LogAction (LogAction), Message, Msg (Msg, msgSeverity, msgStack, msgText), Severity (..))
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (EffectHandler, interpret, localSeqLift, send)
import Prelude hiding (lift)

type Log :: Type -> Effect
data Log msg :: Effect where
    Log :: msg -> Log msg effM ()
    GetLogAction :: Log msg (Eff es) (LogActionEff es msg)

type instance DispatchOf (Log _) = 'Dynamic

type LogActionEff :: [Effect] -> Type -> Type
type LogActionEff es msg = LogAction (Eff es) msg

log ::
    forall msg es.
    (Log msg :> es) =>
    msg ->
    Eff es ()
log = send . Log

logMsg :: (Log Message :> es, HasCallStack) => Severity -> Text -> Eff es ()
logMsg severity msg = log $ Msg{msgStack = callStack, msgSeverity = severity, msgText = msg}

logWithCallStack :: (Log Message :> es) => Severity -> CallStack -> Text -> Eff es ()
logWithCallStack severity stack msg = log $ Msg{msgStack = stack, msgSeverity = severity, msgText = msg}

logDebug :: (Log Message :> es, HasCallStack) => Text -> Eff es ()
logDebug = withFrozenCallStack (logMsg Debug)

logInfo :: (Log Message :> es, HasCallStack) => Text -> Eff es ()
logInfo = withFrozenCallStack (logMsg Info)

logWarning :: (Log Message :> es, HasCallStack) => Text -> Eff es ()
logWarning = withFrozenCallStack (logMsg Warning)

logError :: (Log Message :> es, HasCallStack) => Text -> Eff es ()
logError = withFrozenCallStack (logMsg Error)

getLogAction ::
    forall msg es.
    (Log msg :> es) =>
    Eff es (LogActionEff es msg)
getLogAction =
    send GetLogAction

runLog ::
    forall msg es a.
    LogActionEff es msg ->
    Eff (Log msg : es) a ->
    Eff es a
runLog = runLogM . pure

runLogM ::
    forall msg es a.
    Eff es (LogActionEff es msg) ->
    Eff (Log msg : es) a ->
    Eff es a
runLogM mkLogAction task = do
    LogAction doLog <- mkLogAction
    let handler :: EffectHandler (Log msg) es
        handler localEnv = \case
            Log msg ->
                doLog msg
            GetLogAction -> do
                localSeqLift localEnv \lift -> pure $ LogAction (lift . doLog)
    interpret handler task