{- |
Module: BenchMain
Description: Main benchmark module for ldap-schema-bench

-}
module Main
  ( main
  ) where

import Criterion.Main

import qualified Data.LDAPSchemaBench (benchmarks)

main :: IO ()
main =
  defaultMain
    [ bgroup "All ldap-schema-bench benchmarks"
        [ Data.LDAPSchemaBench.benchmarks
        ]
    ]