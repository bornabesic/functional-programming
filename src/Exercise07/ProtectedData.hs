module Exercise07.ProtectedData where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

-- Given:

data ProtectedData a = ProtectedData {
        password :: String,
        getData :: a
    }

accessData :: String -> ProtectedData a -> Maybe a
accessData s pd =
    if s == password pd then Just (getData pd) else Nothing


-- Implementation with hardcoded password

type Protected s a = MaybeT (Reader (ProtectedData s)) a

access :: String -> Protected a a
access pass = do
    pd <- lift $ ask
    case accessData pass pd of
        (Just d) -> return d
        Nothing -> fail "Password invalid"

run :: (ProtectedData s) -> Protected s a -> Maybe a
run pData pMonad = ((runReader . runMaybeT) pMonad) pData

runExample = run (ProtectedData "pass1234" "secret string")
                 (access "pass1234")

-- Implementation with asking for the password input

type ProtectedIO s a = MaybeT (ReaderT (ProtectedData s) IO) a

accessIO :: ProtectedIO a a
accessIO = do
    passInput <- lift $ lift $ getLine
    pd <- lift $ ask
    case accessData passInput pd of
        (Just d) -> return d
        Nothing -> fail "Password invalid"

runIO :: (ProtectedData s) -> ProtectedIO s a -> IO (Maybe a)
runIO pData pMonad = ((runReaderT. runMaybeT) pMonad) pData

runExampleIO = runIO (ProtectedData "pass1234" "secret string")
                   accessIO