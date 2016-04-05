module Data.GraphQL.StringParser
  ( document
  ) where

import Prelude
  ( Unit
  , unit
  , pure
  , bind
  , negate
  , ($)  , (>)  , (*)  , (+)  , (-) , (/)
  , (==) , (>=) , (<=) , (/=) , (&&), (||), (<>)
  , (<$>), (<*>), (>>=), (<<<)
  )

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Char (toCharCode)
import Data.Foldable (foldr, foldl)
import Data.Functor ((<$), ($>))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.List (List(..), (:), toUnfoldable)
import Data.String (fromCharArray)

import Math (pow) as Math

import Text.Parsing.StringParser (Parser, try, fail)
import Text.Parsing.StringParser.Combinators ((<?>), between, option, fix, many, many1, optionMaybe)
import Text.Parsing.StringParser.String (satisfy, string, oneOf, char, anyDigit)

import Data.GraphQL.AST

  ( Name
  , Alias

  , Document(..)
  , Definition(..)

  , OperationDefinition(..)
  , Node(..)

  , SelectionSet
  , Selection(..)
  , Field(..)

  , Arguments
  , Argument(..)

  , FragmentSpread(..)
  , FragmentDefinition(..)
  , TypeCondition
  , InlineFragment(..)

  , Value(..)
  , ListValue(..)
  , ObjectValue(..)
  , ObjectField(..)
  , DefaultValue

  , Variable(..)
  , VariableDefinitions
  , VariableDefinition(..)

  , Type(..)
  , NamedType(..)
  , ListType(..)
  , NonNullType(..)

  , Directives
  , Directive(..)

  )

-- * Name

-- * Document Parser

document :: Parser Document
document = whitespace
   *> (Document <$> many1 definition)
  <|> (Document <<< pure
                <<< DefinitionOperation
                <<< Query
                <<< Node mempty mempty mempty
                <$> selectionSet)
  <?> "invalid document!"

definition :: Parser Definition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser OperationDefinition
operationDefinition = Query    <$ tok "query"    <*> node
                  <|> Mutation <$ tok "mutation" <*> node
                  <?> "definition error!"

node :: Parser Node
node = Node <$> name
            <*> optempty variableDefinitions
            <*> pure mempty
            <*> selectionSet

variableDefinitions :: Parser VariableDefinitions
variableDefinitions = parens $ many1 variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition <$> variable
                                        <*  sym ':'
                                        <*> type_
                                        <*> optionMaybe defaultValue

defaultValue :: Parser DefaultValue
defaultValue = sym '=' *> value

variable :: Parser Variable
variable = Variable <$ sym '$' <*> name

selectionSet :: Parser SelectionSet
selectionSet = fix \s -> braces $ many1 $ selection s

selection :: Parser SelectionSet -> Parser Selection
selection s = SelectionField <$> field s
    <|> try ( SelectionInlineFragment <$> inlineFragment s )
    <|>       SelectionFragmentSpread <$> fragmentSpread
    <?>       "selection error!"

field :: Parser SelectionSet -> Parser Field
field s = Field <$> optempty (try alias)
                <*> name
                <*> optempty arguments
                <*> optempty directives
                <*> optempty s

alias :: Parser Alias
alias = name <* sym ':'

arguments :: Parser Arguments
arguments = parens $ many1 argument

argument :: Parser Argument
argument = Argument <$> name
                    <*  sym ':'
                    <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
fragmentSpread = FragmentSpread
  <$  tok "..."
  <*> name
  <*> optempty directives

inlineFragment :: Parser SelectionSet -> Parser InlineFragment
inlineFragment s = InlineFragment
  <$  tok "..."
  <*  tok "on"
  <*> optionMaybe typeCondition
  <*> optempty directives
  <*> s

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition = FragmentDefinition
  <$  tok "fragment"
  <*> name
  <*  tok "on"
  <*> typeCondition
  <*> optempty directives
  <*> selectionSet

typeCondition :: Parser TypeCondition
typeCondition = namedType

-- * Values
value :: Parser Value
value = fix \v -> lexeme num
  <|> ValueVariable <$> variable
  <|> ValueBoolean  <$> boolean
  <|> ValueString   <$> stringLiteral
  <|> ValueEnum     <$> name
  <|> ValueList     <$> listValue v
  <|> ValueObject   <$> objectValue v
  <?> "value error!"

num :: Parser Value
num = int >>= \x -> try (ValueFloat <$> fract x)
                <|> try (ValueFloat <$> exp (toNumber x))
                <|> pure (ValueInt x)

-- STRING LITERAL PARSING
stringLiteral :: Parser String
stringLiteral = quotes do
  cs <- many stringChar
  pure $ fromCharArray $ toUnfoldable cs

stringChar :: Parser Char
stringChar = satisfy ( \c -> c /= '"'
                          && c /= '\\'
                     )
         <|> (char '\\' *> escapeChar)

escapeChar :: Parser Char
escapeChar = char '"'
         <|> char '\\'
         <|> char 't' *> pure '\t'
         <|> char 'n' *> pure '\n'

boolean :: Parser Boolean
boolean = tok "true"  *> pure true
      <|> tok "false" *> pure false

listValue :: Parser Value -> Parser ListValue
listValue v = ListValue <$> brackets (many v)

objectValue :: Parser Value -> Parser ObjectValue
objectValue v = ObjectValue <$> braces (many $ objectField v)

objectField :: Parser Value -> Parser ObjectField
objectField v = ObjectField <$> name <* tok ":" <*> v

-- * Directives

directives :: Parser Directives
directives = many1 directive

directive :: Parser Directive
directive = Directive <$ tok "@"
                      <*> name
                      <*> optempty arguments

-- * Type Reference
type_ :: Parser Type
type_  = fix \t -> TypeList    <$> listType t
         <|> try ( TypeNonNull <$> nonNullType t )
         <|>       TypeNamed   <$> namedType
         <?> "type_ error!"

namedType :: Parser NamedType
namedType = NamedType <$> name

nonNullType :: Parser Type -> Parser NonNullType
nonNullType t = NonNullTypeNamed <$> namedType  <* sym '!'
            <|> NonNullTypeList  <$> listType t <* sym '!'
            <?> "nonNullType error!"

listType :: Parser Type -> Parser ListType
listType t = ListType <$> brackets t

-- * Token Parsers

inRange :: Char -> Char -> Char -> Boolean
inRange s e c = s' <= c' && e' >= c'
  where s' = toCharCode s
        e' = toCharCode e
        c' = toCharCode c

-- hopefully this gets optimized by the compiler
nStart :: Char -> Boolean
nStart c = inRange 'a' 'z' c
        || inRange 'A' 'Z' c
        || c == '_'
nLetter :: Char -> Boolean
nLetter c = nStart c
         || inRange '0' '9' c

name :: Parser Name
name = fromCharArray <<< toUnfoldable <$> lexeme do
  c  <- satisfy nStart
  cs <- many $ satisfy nLetter
  pure $ c : cs

tok :: String -> Parser String
tok = lexeme <<< string

sym :: Char -> Parser Char
sym = lexeme <<< char

parens   :: forall a. Parser a -> Parser a
parens    = between (sym '(') (sym ')')

brackets :: forall a. Parser a -> Parser a
brackets  = between (sym '[') (sym ']')

braces   :: forall a. Parser a -> Parser a
braces    = between (sym '{') (sym '}')

quotes   :: forall a. Parser a -> Parser a
quotes    = between (char '"') (sym '"')

optempty :: forall a. (Monoid a) => Parser a -> Parser a
optempty  = option mempty

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* whitespace

whitespace :: Parser Unit
whitespace = do
  many (satisfy insignificant <|> comment)
  pure unit

insignificant :: Char -> Boolean
insignificant c = c == '\n'
               || c == '\r'
               || c == '\t'
               || c == ' '
               || c == ','

comment :: Parser Char
comment = try (char '#' <* many (satisfy \c -> c /= '\n'))

-- number parsing

nat :: Parser Int
nat = do
  digits <- many1 (digitToInt <$> anyDigit)
  pure $ foldl folder 0 digits
  where folder :: Int -> Int -> Int
        folder x d = 10 * x + d

int :: Parser Int
int = do
  sign <- option '+' (char '-')
  whitespace
  n    <- nat
  case sign of
       '-' -> pure $ negate n
       _   -> pure n

fract :: Int -> Parser Number
fract x = try do
    char '.'
    digits <- many1 (digitToInt <$> anyDigit)
    let n = toNumber x + foldl folder 0.0 digits
    exp n <|> pure n
  where folder :: Number -> Int -> Number
        folder acc d = ( acc + toNumber d ) / 10.0

exp :: Number -> Parser Number
exp x = try do
  char 'e' <|> char 'E'
  p <- int
  pure (x * Math.pow 10.0 (toNumber p))

digitToInt :: Char -> Int
digitToInt c = toCharCode c - toCharCode '0'

