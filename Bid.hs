module Bid (Bid (..), start, Bid.decode, Bid.encode, Bid.getMax) where

import Data.ByteString
import Data.Semilattice
import Data.Serialize
import CRDT.Cv.Max (Max (..))
import qualified CRDT.Cv.Max as Max

type Bid = Max Integer

instance Serialize a => Serialize (Max a) where
	put (Max v) = put v
	get = Max <$> get

start :: Bid
start = Max.initial 0

decode :: ByteString -> Either String Bid
decode = Data.Serialize.decode

encode :: Bid -> ByteString
encode = Data.Serialize.encode

getMax :: Max Integer -> Integer
getMax = Max.getMax
