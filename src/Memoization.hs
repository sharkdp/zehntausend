{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Memoization (
    Double()
    ) where

-- See: http://stackoverflow.com/a/2217374/704831

import Data.MemoTrie
import Data.Binary
import qualified Data.ByteString.Lazy as BS

mangle :: Double -> [Int]
mangle = map fromIntegral . BS.unpack . encode

unmangle :: [Int] -> Double
unmangle = decode . BS.pack . map fromIntegral

instance HasTrie Double where
    data Double :->: a = DoubleTrie ([Int] :->: a)
    trie f = DoubleTrie $ trie $ f . unmangle
    untrie (DoubleTrie t) = untrie t . mangle
