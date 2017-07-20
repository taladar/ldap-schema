{- |
Module: Data.LDAPSchemaBench
Description: Benchmarks for Data.LDAPSchema

-}
module Data.LDAPSchemaBench
  ( benchmarks
  ) where

import Criterion.Main

import Data.LDAPSchema

-- Example Benchmark:
--
-- bench "reverse foo" $ nf reverse ("foo" :: String)

benchmarks :: Benchmark
benchmarks =
  bgroup "Data.LDAPSchema"
    [
    ]
