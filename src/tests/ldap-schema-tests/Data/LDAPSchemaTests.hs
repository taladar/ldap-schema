{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Either

import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Text as T
import qualified Data.Text.IO

import Text.Megaparsec

import Data.LDAPSchema

isParseSuccess :: ( Ord t
                  , ShowToken t
                  , ShowErrorComponent e
                  )
               => R.Matcher (Either (ParseError t e) a)
isParseSuccess =
  R.Matcher { R.match = Data.Either.isRight
            , R.description = "Megaparsec parse success"
            , R.describeMismatch = \(Left v) -> parseErrorPretty v
            }

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
    [ testGroup "oidP"
        [ QC.testProperty "random show output is parsed to get identical value" $
            \v -> parse oidP "QC" (T.pack $ show v) QC.=== Right v
        , testCase "Parsing of Utf8String type OID yields expected result" $ do
            assertEqual
              "Utf8String OID"
              (parse oidP "Utf8String OID" "1.3.6.1.4.1.1466.115.121.1.15")
              (Right $ OID $ 1 NonEmpty.:| [3,6,1,4,1,1466,115,121,1,15])
        ]
    , testGroup "attributeNameP"
        [ QC.testProperty "random show output is parsed to get identical value" $
            \v -> parse attributeNameP "QC" (T.pack $ show v) QC.=== Right v
        ]
    , testGroup "attributeP"
        [ testCase "Parsing of sysAdmin attribute from saltation.schema yields expected result" $ do
            assertEqual
              "sysAdmin Attribute"
              (parse attributeP "sysAdmin Attribute" "attributetype ( 1.3.6.1.4.1.42076.1.2.1\n                NAME ( 'systemAdministrator' 'sysAdmin')\n                SUP distinguishedName\n              )")
              (Right $ Attribute
                (OID (1 NonEmpty.:| [3,6,1,4,1,42076,1,2,1]))
                ((AttributeName "systemAdministrator") NonEmpty.:| [AttributeName "sysAdmin"])
                Nothing
                (Just (AttributeName "distinguishedName"))
                Nothing
                False
                Nothing
                Nothing)
        ]
    , testGroup "objectClassNameP"
        [ QC.testProperty "random show output is parsed to get identical value" $
            \v -> parse objectClassNameP "QC" (T.pack $ show v) QC.=== Right v
        ]
    , testGroup "objectClassTypeP"
        [ QC.testProperty "uppercased random show output is parsed to get identical value" $
            \v -> parse objectClassTypeP "QC" (T.toUpper $ T.pack $ show v) QC.=== Right v
        ]
    , testGroup "objectClassP"
        [
        ]
    , testGroup "ldapSchemaP"
        [ testCase "Parsing saltation.schema works" $ do
            content <- Data.Text.IO.readFile "saltation.schema"
            R.expect (parse ldapSchemaP "saltation.schema" content) isParseSuccess
        ]
    ]
