{-# OPTIONS -Wall #-}

-- |
-- Module: Casing
-- Copyright: (c) Marshall Bowers 2019
-- License: MIT
-- Maintainer: elliott.codes@gmail.com
-- Portability: portable
--
-- = Description
--
-- Contains case conversion functions.
module Casing
  ( toCamelCase
  , toPascalCase
  , toSnakeCase
  , toScreamingSnakeCase
  , toKebabCase
  , toTitleCase
  ) where

import qualified Data.Char as Char
import Data.List (intercalate)
import Data.Maybe (catMaybes)

stringToMaybe :: String -> Maybe String
stringToMaybe [] = Nothing
stringToMaybe value = Just value

mapHead :: (a -> a) -> [a] -> [a]
mapHead _mapping [] = []
mapHead mapping (x:xs) = mapping x : xs

mapTail :: (a -> a) -> [a] -> [a]
mapTail _mapping [] = []
mapTail mapping (x:xs) = x : map mapping xs

capitalize :: String -> String
capitalize = mapHead Char.toUpper . mapTail Char.toLower

isSeparator :: Char -> Bool
isSeparator '_' = True
isSeparator '-' = True
isSeparator ' ' = True
isSeparator _ = False

isBoundary :: Char -> Char -> Bool
isBoundary _currentChar nextChar
  | isSeparator nextChar = True
isBoundary currentChar nextChar =
  Char.isLower currentChar && Char.isUpper nextChar

getWords :: String -> [String]
getWords value = reverse $ catMaybes $ map stringToMaybe $ getWords' "" [] value
  where
    getWords' currentWord acc [] = currentWord : acc
    getWords' currentWord acc (singleChar:[]) =
      (currentWord ++ [singleChar]) : acc
    getWords' currentWord acc (currentChar:nextChar:remainingChars) =
      let appendCurrentChar word =
            if isSeparator currentChar
              then word
              else word ++ [currentChar]
          (currentWord', acc') =
            if isBoundary currentChar nextChar
              then ("", appendCurrentChar currentWord : acc)
              else if all Char.isUpper currentWord &&
                      Char.isUpper currentChar && Char.isLower nextChar
                     then (appendCurrentChar "", currentWord : acc)
                     else (appendCurrentChar currentWord, acc)
          remainingChars' =
            if not $ isSeparator nextChar
              then nextChar : remainingChars
              else remainingChars
       in getWords' currentWord' acc' remainingChars'

-- | Converts the given string to camelCase.
--
-- In camelCase each word starts with an uppercase letter except for the first
-- word, which starts with a lowercase letter.
--
-- >>> toCamelCase "Hello World"
-- "helloWorld"
--
-- >>> toCamelCase "Player ID"
-- "playerId"
--
-- >>> toCamelCase "XMLHttpRequest"
-- "xmlHttpRequest"
toCamelCase :: String -> String
toCamelCase =
  intercalate "" . mapTail capitalize . mapHead (map Char.toLower) . getWords

-- | Converts the given string to PascalCase.
--
-- In PascalCase the first letter of each word is uppercase.
--
-- >>> toPascalCase "Hello World"
-- "HelloWorld"
--
-- >>> toPascalCase "Player ID"
-- "PlayerId"
--
-- >>> toPascalCase "XMLHttpRequest"
-- "XmlHttpRequest"
toPascalCase :: String -> String
toPascalCase = intercalate "" . map capitalize . getWords

-- | Converts the given string to snake_case.
--
-- In snake_case all letters are lowercase and each word is separated by an
-- underscore ("_").
--
-- >>> toSnakeCase "Hello World"
-- "hello_world"
--
-- >>> toSnakeCase "Player ID"
-- "player_id"
--
-- >>> toSnakeCase "XMLHttpRequest"
-- "xml_http_request"
toSnakeCase :: String -> String
toSnakeCase = intercalate "_" . map (map Char.toLower) . getWords

-- | Converts the given string to SCREAMING_SNAKE_CASE.
--
-- In SCREAMING_SNAKE_CASE all letters are uppercase and each word is separated
-- by an underscore ("_").
--
-- >>> toScreamingSnakeCase "Hello World"
-- "HELLO_WORLD"
--
-- >>> toScreamingSnakeCase "Player ID"
-- "PLAYER_ID"
--
-- >>> toScreamingSnakeCase "XMLHttpRequest"
-- "XML_HTTP_REQUEST"
toScreamingSnakeCase :: String -> String
toScreamingSnakeCase = intercalate "_" . map (map Char.toUpper) . getWords

-- | Converts the given string to kebab-case.
--
-- In kebab-case all letters are lowercase and each word is separated by a
-- hyphen ("-").
--
-- >>> toKebabCase "Hello World"
-- "hello-world"
--
-- >>> toKebabCase "Player ID"
-- "player-id"
--
-- >>> toKebabCase "XMLHttpRequest"
-- "xml-http-request"
toKebabCase :: String -> String
toKebabCase = intercalate "-" . map (map Char.toLower) . getWords

-- | Converts the given string to Title Case.
--
-- In Title Case the first letter of each word is uppercase and each word is
-- separated by a space (" ").
--
-- >>> toTitleCase "Hello World"
-- "Hello World"
--
-- >>> toTitleCase "Player ID"
-- "Player Id"
--
-- >>> toTitleCase "XMLHttpRequest"
-- "Xml Http Request"
toTitleCase :: String -> String
toTitleCase = intercalate " " . map capitalize . getWords
