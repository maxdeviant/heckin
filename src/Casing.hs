{-# OPTIONS -Wall #-} 

module Casing
    ( toCamelCase
    , toPascalCase
    , toSnakeCase
    , toScreamingSnakeCase
    , toKebabCase
    , toTitleCase
    ) where

import Data.Char as Char
import Data.List (intercalate)
import Data.Maybe (catMaybes)

isBoundary :: Char -> Char -> Bool
isBoundary _currentChar '_' = True
isBoundary currentChar nextChar =
    Char.isLower currentChar && Char.isUpper nextChar

getWords' :: String -> [String] -> [Char] -> [String]
getWords' currentWord acc [] = currentWord : acc
getWords' currentWord acc (singleChar : []) = (currentWord ++ [singleChar]) : acc
getWords' currentWord acc (currentChar : nextChar : remainingChars) =
    let
        (currentWord', acc') =
            if isBoundary currentChar nextChar then
                ("", (currentWord ++ [currentChar]) : acc)
            else if all Char.isUpper currentWord && Char.isUpper currentChar && Char.isLower nextChar then
                ([currentChar], currentWord : acc)
            else
                (currentWord ++ [currentChar], acc)

        remainingChars' =
            if nextChar /= '_' then nextChar : remainingChars
            else remainingChars
    in
        getWords' currentWord' acc' remainingChars'

stringToMaybe :: String -> Maybe String
stringToMaybe [] = Nothing
stringToMaybe value = Just value

getWords :: String -> [String]
getWords value =
    reverse $ catMaybes $ map stringToMaybe $ getWords' "" [] value

mapHead :: (a -> a) -> [a] -> [a]
mapHead _mapping [] = []
mapHead mapping (x:xs) = mapping x : xs

mapTail :: (a -> a) -> [a] -> [a]
mapTail _mapping [] = []
mapTail mapping (x:xs) = x : map mapping xs

makePascalCase :: String -> String
makePascalCase =
    mapHead Char.toUpper . mapTail Char.toLower

makeCamelCase :: String -> String
makeCamelCase =
    map Char.toLower

toCamelCase :: String -> String
toCamelCase =
    intercalate "" . mapTail makePascalCase . mapHead makeCamelCase . getWords

toPascalCase :: String -> String
toPascalCase =
    intercalate "" . map makePascalCase . getWords

toSnakeCase :: String -> String
toSnakeCase =
    intercalate "_" . map (map Char.toLower) . getWords

toScreamingSnakeCase :: String -> String
toScreamingSnakeCase =
    intercalate "_" . map (map Char.toUpper) . getWords

toKebabCase :: String -> String
toKebabCase =
    intercalate "-" . map (map Char.toLower) . getWords

toTitleCase :: String -> String
toTitleCase =
    intercalate " " . map makePascalCase . getWords
