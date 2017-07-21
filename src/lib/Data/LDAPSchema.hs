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

import           Control.Monad (void)
import           Data.Either (partitionEithers)
import           Data.List (intercalate, sort)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isNothing,fromJust)
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
      } deriving (Eq,Ord)

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
                } deriving (Eq, Ord)

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
  s <- T.singleton <$> oneOf (['a'..'z'] ++ ['A'..'Z'])
  u <- T.pack <$> many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-']))
  return $ AttributeName $ s <> u

data AttributeSyntax =
  AttributeSyntax { syntaxOID :: OID
                  , syntaxLength :: Maybe Integer
                  } deriving (Eq, Ord, Show)

instance QC.Arbitrary AttributeSyntax where
  arbitrary =
    AttributeSyntax <$> QC.arbitrary <*> (QC.suchThat QC.arbitrary (\x -> if isNothing x then True else (fromJust x) > 0))

data Attribute =
  Attribute { attributeOID :: OID
            , attributeNames :: NonEmpty AttributeName
            , attributeDescription :: Maybe Text
            , attributeSuperior :: Maybe AttributeName
            , attributeSyntax :: Maybe AttributeSyntax
            , attributeSingleValue :: Bool
            , attributeCollective :: Bool
            , attributeEqualityMatch :: Maybe Text
            , attributeSubstringMatch :: Maybe Text
            , attributeOrdering :: Maybe Text
            } deriving (Eq, Show)

data AttributeParameterResult =
    AttributeDescriptionResult Text
  | AttributeSuperiorResult AttributeName
  | AttributeSyntaxResult AttributeSyntax
  | AttributeSingleValueResult Bool
  | AttributeCollectiveResult Bool
  | AttributeEqualityMatchResult Text
  | AttributeSubstringMatchResult Text
  | AttributeOrderingResult Text
    deriving (Eq, Ord, Show)

attributeDescriptionP :: Parser AttributeParameterResult
attributeDescriptionP = label "AttributeDescriptionResult" $ AttributeDescriptionResult <$> do
  string "DESC"
  space
  char '\''
  T.pack <$> manyTill (noneOf "'\n") (char '\'')

attributeSuperiorP :: Parser AttributeParameterResult
attributeSuperiorP = label "AttributeSuperiorResult" $ AttributeSuperiorResult <$> do
  string "SUP"
  space
  attributeNameP

attributeSyntaxP :: Parser AttributeParameterResult
attributeSyntaxP = label "AttributeSyntaxResult" $ AttributeSyntaxResult <$> do
  string "SYNTAX"
  space
  syntaxOID <- oidP
  syntaxLength <- optional $ do
    char '{'
    i <- integer
    char '}'
    return i
  return $ AttributeSyntax{..}

attributeSingleValueP :: Parser AttributeParameterResult
attributeSingleValueP = label "AttributeSingleValueResult" $ AttributeSingleValueResult <$> do
  string "SINGLE-VALUE"
  return True

attributeCollectiveP :: Parser AttributeParameterResult
attributeCollectiveP = label "AttributeCollectiveResult" $ AttributeCollectiveResult <$> do
  string "COLLECTIVE"
  return True

attributeEqualityMatchP :: Parser AttributeParameterResult
attributeEqualityMatchP = label "AttributeEqualityMatchResult" $ AttributeEqualityMatchResult <$> do
  string "EQUALITY"
  space
  T.pack <$> some alphaNumChar

attributeSubstringMatchP :: Parser AttributeParameterResult
attributeSubstringMatchP = label "AttributeSubstringMatchResult" $ AttributeSubstringMatchResult <$> do
  string "SUBSTR"
  space
  T.pack <$> some alphaNumChar

attributeOrderingP :: Parser AttributeParameterResult
attributeOrderingP = label "AttributeOrderingResult" $ AttributeOrderingResult <$> do
  string "ORDERING"
  space
  T.pack <$> some alphaNumChar

attributeP :: Parser Attribute
attributeP = label "Attribute" $ do
  string' "attributetype"
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
            (char ')')
            (sepEndBy1
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
    parameters <-
      endBy (choice
        [ attributeDescriptionP
        , attributeSuperiorP
        , attributeSyntaxP
        , attributeSingleValueP
        , attributeCollectiveP
        , attributeEqualityMatchP
        , attributeSubstringMatchP
        , attributeOrderingP
        ]) space
    let sortedParameters = Data.List.sort parameters
        (sortedParameters2, attributeDescription) =
          case sortedParameters of
            (AttributeDescriptionResult v):xs ->
              (xs, Just v)
            xs ->
              (xs, Nothing)
        (sortedParameters3, attributeSuperior) =
          case sortedParameters2 of
            (AttributeSuperiorResult v):xs ->
              (xs, Just v)
            xs ->
              (xs, Nothing)
        (sortedParameters4, attributeSyntax) =
          case sortedParameters3 of
            (AttributeSyntaxResult v):xs ->
              (xs, Just v)
            xs ->
              (xs, Nothing)
        (sortedParameters5, attributeSingleValue) =
          case sortedParameters4 of
            (AttributeSingleValueResult v):xs ->
              (xs, v)
            xs ->
              (xs, False)
        (sortedParameters6, attributeCollective) =
          case sortedParameters5 of
            (AttributeCollectiveResult v):xs ->
              (xs, v)
            xs ->
              (xs, False)
        (sortedParameters7, attributeEqualityMatch) =
          case sortedParameters6 of
            (AttributeEqualityMatchResult v):xs ->
              (xs, Just v)
            xs ->
              (xs, Nothing)
        (sortedParameters8, attributeSubstringMatch) =
          case sortedParameters7 of
            (AttributeSubstringMatchResult v):xs ->
              (xs, Just v)
            xs ->
              (xs, Nothing)
        attributeOrdering =
          case sortedParameters8 of
            (AttributeOrderingResult v):_ ->
              Just v
            _ ->
              Nothing
    return $ Attribute{..}

newtype ObjectClassName =
  ObjectClassName { unObjectClassName :: Text
                  } deriving (Eq, Ord)

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
  s <- T.singleton <$> oneOf (['a'..'z'] ++ [ 'A'..'Z' ])
  u <- T.pack <$> many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-']))
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
              , objectClassSuperior :: [ObjectClassName]
              , objectClassRequiredAttributes :: [AttributeName]
              , objectClassOptionalAttributes :: [AttributeName]
              } deriving (Eq, Show)

objectClassP :: Parser ObjectClass
objectClassP = label "ObjectClass" $ do
  string' "objectclass"
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
            (char ')')
            (sepEndBy1
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
    objectClassSuperior <- option [] $ do
      string "SUP"
      space
      choice
        [ between (char '(' >> space) (space >> char ')') $ do
            sepBy1
              objectClassNameP
              (try $ space >> char '$' >> space)
        , pure <$> objectClassNameP
        ]
    space
    objectClassType <- objectClassTypeP
    space
    objectClassRequiredAttributes <- option [] $ do
      string "MUST"
      space
      choice
        [ between (char '(' >> space) (space >> char ')') $ do
            sepBy1
              attributeNameP
              (try $ space >> char '$' >> space)
        , pure <$> attributeNameP
        ]
    space
    objectClassOptionalAttributes <- option [] $ do
      string "MAY"
      space
      choice
        [ between (char '(' >> space) (space >> char ')') $ do
            sepBy1
              attributeNameP
              (try $ space >> char '$' >> space)
        , pure <$> attributeNameP
        ]
    space
    return $ ObjectClass{..}

data LDAPSchema =
  LDAPSchema { ldapSchemaObjectClasses :: [ObjectClass]
             , ldapSchemaAttributes :: [Attribute]
             } deriving (Eq, Show)

comment :: Parser ()
comment = label "Comment" $ do
  char '#'
  void $ manyTill (noneOf "\n") newline

uninteresting :: Parser ()
uninteresting =  label "Comment or whitespace lines" $ do
  skipMany (eitherP comment (void spaceChar))

ldapSchemaP :: Parser LDAPSchema
ldapSchemaP = label "LDAPSchema" $ do
  res <- manyTill
    ((try uninteresting) >> (eitherP (objectClassP) (attributeP)))
    (try $ space >> eof)
  return $ (uncurry LDAPSchema) $ partitionEithers res

