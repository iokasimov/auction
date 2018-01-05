module Node (run, receiving) where

import Data.Monoid
import Data.Either
import Data.Semilattice
import Control.Lens
import Control.Monad.Trans.Class
import Control.Concurrent.Async (async)
import Control.Monad (forever, sequence_, void)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Cont
import Control.Error.Util (hush)
import Network.Simple.TCP as TCP

import Bid (Bid (..))
import qualified Bid

type Port = Int

run :: Port -> ContT () IO (TCP.Socket, TCP.SockAddr)
run port = ContT $ \handle -> TCP.serve
    (TCP.Host "127.0.0.1") (show port) handle

receiving :: (TCP.Socket, TCP.SockAddr) -> StateT Bid IO ()
receiving (socket, address) = get >>= \oldbid -> do
    lift $ print $ "OLD: " <> (show $ Bid.getMax oldbid)
    received <- lift $ TCP.recv socket 1024
    case received of
        Nothing -> lift $ print "got nothing..."
        Just bytes -> case Bid.decode bytes of
            Left err -> lift $ print err
            Right newbid -> do
                put $ newbid `merge` oldbid
                get >>= lift . print . mappend "NEW: " . show . Bid.getMax
