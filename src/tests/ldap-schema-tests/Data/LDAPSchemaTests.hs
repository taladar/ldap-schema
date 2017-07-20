{- |
Module: Data.LDAPSchemaTests
Description: Tests for Data.LDAPSchema

-}
module Data.LDAPSchemaTests
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()
import qualified Control.Rematch as R
import qualified Test.Rematch.HUnit as R

import Data.LDAPSchema

-- Example tests
--
-- HUnit without Rematch
-- testCase "example test case" $
--   assertEqual "description" foo (reverse "oof")
--
-- HUnit with Rematch
-- testCase "Decoding foo.json works without an error" $ do
--   content <- LBS.readFile "foo.json"
--   R.expect (eitherDecode' content :: Either String ExpectedType) R.isRight
--
-- QuickCheck
-- QC.testProperty "smart constructor does not allow construction of display name from single character Text" $
--   \c -> isNothing (mkDisplayName $ T.singleton c)
--
-- QuickCheck with restrictions
-- QC.testProperty "refuses to construct DisplayName for 31+ character Text" $
--   QC.forAll (QC.suchThat QC.arbitrary (\t -> T.length t > 30)) $
--     isNothing . mkDisplayName

tests :: TestTree
tests =
  testGroup "Data.LDAPSchema"
    [
    ]
