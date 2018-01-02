module Node (Node (..), socket, bid, initialization, receiving, broadcasting) where

import Data.Monoid
import Data.Either
import Data.Semilattice
import Control.Lens
import Control.Monad.Trans.Class
import Control.Concurrent.Async (async)
import Control.Monad (forever, sequence_, void)
import Control.Monad.Trans.State.Strict
import Network.Socket (Family (..), PortNumber,
    SockAddr (..), Socket (..), SocketType (..))
import qualified Network.Socket as Socket hiding (recv, send)
import qualified Network.Socket.ByteString as Socket

import Bid (Bid (..))
import qualified Bid

--------------------------------------------------------------------------------

data Node = Node Socket Bid

instance Show Node where
    show (Node socket' bid') = "[#] Node: " <>
        (show socket') <> ", " <> (show bid')

socket :: Lens Node Node Socket Socket
socket modify (Node socket' bid') = (\new -> Node new bid') <$> modify socket'

bid :: Lens Node Node Bid Bid
bid modify (Node socket' bid') = (\new -> Node socket' new) <$> modify bid'

--------------------------------------------------------------------------------

initialization :: PortNumber -> IO Node
initialization portnum = do
    s <- Socket.socket AF_INET Stream Socket.defaultProtocol
    Socket.bind s $ SockAddrInet portnum 0x0100007f
    Socket.listen s Socket.sOMAXCONN
    pure $ Node s Bid.start

receiving :: StateT Node IO ()
receiving = forever $ get >>= \node -> do
    (from_socket, _) <- lift $ Socket.accept $ node ^. socket
    bytes <- lift $ Socket.recv from_socket 4096
    case Bid.decode bytes of
        Right newbid -> do
            lift $ print newbid
            if newbid <= (node ^. bid) then pure ()
            else put $ node & bid .~ merge newbid (node ^. bid)
        Left err -> lift $ print err

-- notify nodes about new bid
broadcasting :: Traversable t => Node -> t Node -> IO ()
broadcasting (Node socket' bid') nodes = void $ traverse sending nodes where

    sending :: Node -> IO ()
    sending node = do
        port <- Socket.socketPort $ node ^. socket
        let address = Socket.SockAddrInet port 0x0100007f
        Socket.connect (node ^. socket) address
        void $ Socket.send (node ^. socket) $ Bid.encode (node ^. bid)
