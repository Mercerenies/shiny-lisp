
module Shiny.Case(upperCase, isUpperCase, toUpperCase, toLowerCase) where

import Data.List

upperCase :: [Char]
upperCase = "QWERTYUIOPASDFGHJKLZXCVBNM|<>?@$^*+_"

lowerCase :: [Char]
lowerCase = "qwertyuiopasdfghjklzxcvbnm`,;/!#%&=-"

isUpperCase :: Char -> Bool
isUpperCase x = x `elem` upperCase

toUpperCase :: Char -> Char
toUpperCase ch = case elemIndex ch lowerCase of
                   Nothing -> ch
                   Just n -> upperCase !! n

toLowerCase :: Char -> Char
toLowerCase ch = case elemIndex ch upperCase of
                   Nothing -> ch
                   Just n -> lowerCase !! n
