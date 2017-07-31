
module Shiny.Case(isUpperCase, isLowerCase, toUpperCase, toLowerCase) where

import Data.List
import Data.Char(ord)
import qualified Data.Char as Char

upperCase :: [Char]
upperCase = "QWERTYUIOPASDFGHJKLZXCVBNM|<>?@$^*+_"

lowerCase :: [Char]
lowerCase = "qwertyuiopasdfghjklzxcvbnm`,;/!#%&=-"

isUpperCase :: Char -> Bool
isUpperCase x | ord x >= 128 = Char.isUpper x
isUpperCase x = x `elem` upperCase

-- NB: This differs from the Unicode definition of lowercase.
--     ShinyLisp considers Unicode characters to be uppercase if
--     Unicode considers them either uppercase or titlecase, and
--     it considers all others to be lowercase.
isLowerCase :: Char -> Bool
isLowerCase = not . isUpperCase

toUpperCase :: Char -> Char
toUpperCase ch | ord ch >= 128 = Char.toUpper ch
toUpperCase ch = case elemIndex ch lowerCase of
                   Nothing -> ch
                   Just n -> upperCase !! n

toLowerCase :: Char -> Char
toLowerCase ch | ord ch >= 128 = Char.toLower ch
toLowerCase ch = case elemIndex ch upperCase of
                   Nothing -> ch
                   Just n -> lowerCase !! n
