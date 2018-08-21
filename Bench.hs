{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Criterion.Main
import Paper

memoFix n = $$(result) !! n

memoFix_case n = $$(result_case) n

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
                      ]
  , bgroup "fib" [ bench "naive" $ whnf fib 1000
                 , bench "naive-1_000_000" $ whnf fib 100000
                 , bench "fib-100" $ whnf memoFix 1000
                 , bench "fib_case" $ whnf memoFix_case 1000
                 , bench "fib_case 1_000_000" $ whnf memoFix_case 100000] ]
