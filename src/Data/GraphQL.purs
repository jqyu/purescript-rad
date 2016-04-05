module Data.GraphQL
  ( toAST
  , module Data.GraphQL.AST
  ) where

import Data.Either (Either)
import Data.GraphQL.AST (Document)
import Data.GraphQL.StringParser (document)

import Text.Parsing.StringParser (runParser, ParseError)

toAST :: String -> Either ParseError Document
toAST = runParser document
