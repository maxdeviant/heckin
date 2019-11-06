{-# OPTIONS -Wall #-} 

module Casing
    ( toCamelCase
    , toPascalCase
    ) where

import Data.Char as Char
import Data.List (intercalate)

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

getWords :: String -> [String]
getWords value =
    reverse $ getWords' "" [] value

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
