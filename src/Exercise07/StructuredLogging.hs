module Exercise07.StructuredLogging where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

import Data.Time.Clock.POSIX
import System.Posix.Unistd

-- Data definition

data Item = Msg String POSIXTime |
            -- ^ Message and the timestamp
            Section String [Item] (POSIXTime, POSIXTime)
            -- ^ Section containing messages with before and after timestamp
            deriving (Show,Eq)

type Log = [Item]

type Logging a = WriterT Log (IO) a

-- Logs the message
log :: (Show t) => t -> Logging ()
log msg = do
    timestamp <- lift $ getPOSIXTime
    tell [Msg (show msg) timestamp]

-- Executes the monad and add its log to a section
with_section :: String -> Logging a -> Logging a
with_section section logMonad = do
    timestampBefore <- lift $ getPOSIXTime
    (val, logged) <- lift $ runLogging logMonad
    timestampAfter <- lift $ getPOSIXTime
    tell [Section section logged
            (timestampBefore, timestampAfter)
        ]
    return val

-- Run the logger
runLogging :: Logging a -> IO (a, Log)
runLogging logMonad = runWriterT logMonad




-- with_section using sleep for demonstration
sleepingSection5 :: Logging a -> Logging a
sleepingSection5 logMonad = do
    timestampBefore <- lift $ getPOSIXTime
    (val, logged) <- lift $ runLogging logMonad
    lift $ sleep 5 -- sleep for 5 seconds
    timestampAfter <- lift $ getPOSIXTime
    tell [Section "SLEEPING SECTION 5" logged
            (timestampBefore, timestampAfter)
        ]
    return val