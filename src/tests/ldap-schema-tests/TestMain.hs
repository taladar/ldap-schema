{- |
Module: TestMain
Description: Main test module for ldap-schema-tests

-}
module Main
  ( main
  ) where

import Test.Tasty
import Test.Tasty.Ingredients.Basic (consoleTestReporter, listingTests)
import Test.Tasty.Runners.Html (htmlRunner)
import Test.Tasty.Runners.AntXML (antXMLRunner)

import qualified Data.LDAPSchemaTests (tests)

main :: IO ()
main =
  defaultMainWithIngredients
    [ listingTests
    , consoleTestReporter
    , htmlRunner
    , antXMLRunner
    ] $
    testGroup "All ldap-schema-tests tests"
      [ Data.LDAPSchemaTests.tests
      ]
