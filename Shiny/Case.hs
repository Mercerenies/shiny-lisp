
module Shiny.Case(upperCase, isUpperCase, toLowerCase) where

import Data.List

upperCase :: [Char]
upperCase = "QWERTYUIOPASDFGHJKLZXCVBNM~<>?@$^*_|"

lowerCase :: [Char]
lowerCase = "qwertyuiopasdfghjklzxcvbnm`,;/!#%&=\\"

isUpperCase :: Char -> Bool
isUpperCase x = x `elem` upperCase

toLowerCase :: Char -> Char
toLowerCase ch = case elemIndex ch upperCase of
                   Nothing -> ch
                   Just n -> lowerCase !! n
