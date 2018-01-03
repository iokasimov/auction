import Control.Lens
import Control.Monad (void)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async)
import Control.Monad.Trans.State.Strict

import qualified Node

main = do
    client1 <- Node.initialization 8081
    client2 <- Node.initialization 8082
    client3 <- Node.initialization 8083
    client4 <- Node.initialization 8084
    client5 <- Node.initialization 8085
    -- just wait 10 seconds
    threadDelay 10000000
