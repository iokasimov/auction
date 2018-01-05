import Control.Monad (void)
import Control.Concurrent.Async (async)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Trans.Cont (runContT)
import Network.Simple.TCP as TCP

import qualified Node
import qualified Bid

main = do

    async $ runContT (Node.run 8081) $ \args ->
        void $ execStateT (Node.receiving args) $ (+1000) <$> Bid.start

    TCP.connect "127.0.0.1" "8081" $ \(socket, address) -> do
        TCP.send socket $ Bid.encode $ (+1001) <$> Bid.start
