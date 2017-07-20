{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module: Data.LDAPSchema
Description: Data types to represent LDAP schema and parsers for them

FIXME: Fill in long description for Data.LDAPSchema
-}
module Data.LDAPSchema
  ( OID(..)
  , oidP
  , AttributeName(..)
  , attributeNameP
  , Attribute(..)
  , attributeP
  , ObjectClassName(..)
  , objectClassNameP
  , ObjectClassType(..)
  , objectClassTypeP
  , ObjectClass(..)
  , objectClassP
  , LDAPSchema(..)
  , ldapSchemaP
  ) where

import           Data.Either (partitionEithers)
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Lexer (integer)
import           Text.Megaparsec.Text

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Instances ()

newtype OID =
  OID { unOID :: NonEmpty Integer
      } deriving Eq

instance Show OID where
  show (OID xs) = intercalate "." (NonEmpty.toList $ NonEmpty.map show xs)

instance QC.Arbitrary OID where
  arbitrary = QC.sized $ \n ->
    OID <$> do
      k <- QC.choose (1, max 1 n)
      l <- sequence [ QC.suchThat QC.arbitrary (>=0) | _ <- [0..(k-1)] ]
      return $ NonEmpty.fromList l

oidP :: Parser OID
oidP = label "OID" $ do
  is <- sepBy1 integer (char '.')
  return $ OID $ NonEmpty.fromList is

newtype AttributeName =
  AttributeName { unAttributeName :: Text
                } deriving (Eq)

instance Show AttributeName where
  show (AttributeName v) = T.unpack v

instance QC.Arbitrary AttributeName where
  arbitrary = QC.sized $ \n ->
    AttributeName <$> do
      k <- QC.choose (1, max 1 n)
      s <- QC.elements ['a'..'z']
      u <- sequence [ QC.elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) | _ <- [0..(k-1)] ]
      return $ T.singleton s <> T.pack u

attributeNameP :: Parser AttributeName
attributeNameP = label "AttributeName" $ do
  s <- T.singleton <$> oneOf ['a'..'z']
  u <- T.pack <$> many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  return $ AttributeName $ s <> u

data Attribute =
  Attribute { attributeOID :: OID
            , attributeNames :: NonEmpty AttributeName
            , attributeDescription :: Maybe Text
            , attributeSuperior :: Maybe AttributeName
            , attributeSyntax :: Maybe OID -- TODO: apparently has optional max length
            , attributeSingleValue :: Bool
            , attributeEqualityMatch :: Maybe Text
            , attributeSubstringMatch :: Maybe Text
            } deriving (Eq, Show)

attributeP ::Parser Attribute
attributeP = label "Attribute" $ do
  string "attributetype"
  space
  between (char '(') (char ')') $ do
    space
    attributeOID <- oidP
    space
    string "NAME"
    space
    attributeNames <- NonEmpty.fromList <$>
      choice
        [ between
            (char '(' >> space)
            (space >> char ')')
            (sepBy1
              (between
                (char '\'')
                (char '\'')
                attributeNameP
              )
              space
            )
        , between
            (char '\'')
            (char '\'')
            (pure <$> attributeNameP)
        ]
    space
    attributeDescription <- optional $ do
      string "DESC"
      space
      char '\''
      T.pack <$> manyTill anyChar (char '\'')
    space
    attributeSuperior <- optional $ do
      string "SUP"
      space
      attributeNameP
    space
    attributeSyntax <- optional (string "SYNTAX" >> oidP)
    space
    attributeSingleValue <- option False $ do
      string "SINGLE-VALUE"
      return True
    space
    attributeEqualityMatch <- optional $ do
      string "EQUALITY"
      space
      T.pack <$> some alphaNumChar
    space
    attributeSubstringMatch <- optional $ do
      string "SUBSTR"
      space
      T.pack <$> some alphaNumChar
    return $ Attribute{..}

newtype ObjectClassName =
  ObjectClassName { unObjectClassName :: Text
                  } deriving (Eq)

instance Show ObjectClassName where
  show (ObjectClassName v) = T.unpack v

instance QC.Arbitrary ObjectClassName where
  arbitrary = QC.sized $ \n ->
    ObjectClassName <$> do
      k <- QC.choose (1, max 1 n)
      s <- QC.elements ['a'..'z']
      u <- sequence [ QC.elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) | _ <- [0..(k-1)] ]
      return $ T.singleton s <> T.pack u

objectClassNameP :: Parser ObjectClassName
objectClassNameP = label "ObjectClassName" $ do
  s <- T.singleton <$> oneOf ['a'..'z']
  u <- T.pack <$> many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  return $ ObjectClassName $ s <> u

data ObjectClassType =
    Structural
  | Auxiliary
  | Abstract
    deriving (Eq, Enum, Bounded, Read, Show)

instance QC.Arbitrary ObjectClassType where
  arbitrary = QC.elements [minBound..maxBound]

objectClassTypeP :: Parser ObjectClassType
objectClassTypeP = label "ObjectClassType" $ do
  choice
    [ string "STRUCTURAL" >> return Structural
    , string "AUXILIARY"  >> return Auxiliary
    , string "ABSTRACT"   >> return Abstract
    ]

data ObjectClass =
  ObjectClass { objectClassOID :: OID
              , objectClassNames :: NonEmpty ObjectClassName
              , objectClassDescription :: Maybe Text
              , objectClassType :: ObjectClassType
              , objectClassSuperior :: Maybe ObjectClassName
              , objectClassRequiredAttributes :: [AttributeName]
              , objectClassOptionalAttributes :: [AttributeName]
              } deriving (Eq, Show)

objectClassP :: Parser ObjectClass
objectClassP = label "ObjectClass" $ do
  string "objectclass"
  space
  between (char '(') (char ')') $ do
    space
    objectClassOID <- oidP
    space
    string "NAME"
    space
    objectClassNames <- NonEmpty.fromList <$>
      choice
        [ between
            (char '(' >> space)
            (space >> char ')')
            (sepBy1
              (between
                (char '\'')
                (char '\'')
                objectClassNameP
              )
              space
            )
        , between
            (char '\'')
            (char '\'')
            (pure <$> objectClassNameP)
        ]
    space
    objectClassDescription <- optional $ do
      string "DESC"
      space
      char '\''
      T.pack <$> manyTill anyChar (char '\'')
    space
    objectClassSuperior <- optional $ do
      string "SUP"
      space
      objectClassNameP
    space
    objectClassType <- objectClassTypeP
    space
    objectClassRequiredAttributes <- option [] $ do
      string "MUST"
      space
      between (char '(' >> space) (space >> char ')') $ do
        sepBy1
          attributeNameP
          (try $ space >> char '$' >> space)
    space
    objectClassOptionalAttributes <- option [] $ do
      string "MAY"
      space
      between (char '(' >> space) (space >> char ')') $ do
        sepBy1
          attributeNameP
          (try $ space >> char '$' >> space)
    return $ ObjectClass{..}

data LDAPSchema =
  LDAPSchema { ldapSchemaObjectClasses :: [ObjectClass]
             , ldapSchemaAttributes :: [Attribute]
             } deriving (Eq, Show)

ldapSchemaP :: Parser LDAPSchema
ldapSchemaP = label "LDAPSchema" $ do
  res <- manyTill
    (space >> (eitherP (objectClassP) (attributeP)))
    (try $ space >> eof)
  return $ (uncurry LDAPSchema) $ partitionEithers res

