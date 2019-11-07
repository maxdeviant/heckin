{-# OPTIONS -Wall #-}

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

getWords' :: String -> [String] -> [Char] -> [String]
getWords' currentWord acc [] = currentWord : acc
getWords' currentWord acc (singleChar:[]) = (currentWord ++ [singleChar]) : acc
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

stringToMaybe :: String -> Maybe String
stringToMaybe [] = Nothing
stringToMaybe value = Just value

getWords :: String -> [String]
getWords value = reverse $ catMaybes $ map stringToMaybe $ getWords' "" [] value

mapHead :: (a -> a) -> [a] -> [a]
mapHead _mapping [] = []
mapHead mapping (x:xs) = mapping x : xs

mapTail :: (a -> a) -> [a] -> [a]
mapTail _mapping [] = []
mapTail mapping (x:xs) = x : map mapping xs

capitalize :: String -> String
capitalize = mapHead Char.toUpper . mapTail Char.toLower

toCamelCase :: String -> String
toCamelCase =
  intercalate "" . mapTail capitalize . mapHead (map Char.toLower) . getWords

toPascalCase :: String -> String
toPascalCase = intercalate "" . map capitalize . getWords

toSnakeCase :: String -> String
toSnakeCase = intercalate "_" . map (map Char.toLower) . getWords

toScreamingSnakeCase :: String -> String
toScreamingSnakeCase = intercalate "_" . map (map Char.toUpper) . getWords

toKebabCase :: String -> String
toKebabCase = intercalate "-" . map (map Char.toLower) . getWords

toTitleCase :: String -> String
toTitleCase = intercalate " " . map capitalize . getWords
