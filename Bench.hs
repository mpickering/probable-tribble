module Main where

import Criterion.Main
import Paper

main :: IO ()
main = defaultMain [
  bgroup "coinChange" [ bench "naive" $ whnf change 50
                      , bench "naive-2" $ whnf change2 10000
                      , bench "vector" $ whnf changeVector 50
                      , bench "vector-2" $ whnf changeVector2 10000
                      , bench "unrolled" $ whnf changeSpecialised 50
                      , bench "unrolled-2" $ whnf changeSpecialised2 10000
                      , bench "so-31487832" $ whnf (flip count' coins) 10000
                      , bench "dyna" $ whnf chain_dyna 10000
                      ] ]
