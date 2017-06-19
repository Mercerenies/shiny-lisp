
module Shiny.Vars(Var(..), argumentNames, argListName, argumentBindings, isImplicitlyGlobal) where

-- TODO Unicode capitalization support

newtype Var = Var { getVar :: String }
    deriving (Show, Read, Eq, Ord)

argumentNames :: [Var]
argumentNames = let base = ["x", "y", "z"]
                    base' = ["xx", "yy", "zz"]
                    suffixes = do
                      g <- "" : suffixes
                      f <- ['a'..'z']
                      return $ g ++ [f]
                in map Var $ base ++ base' ++ concatMap (\x -> map (++ x) base') suffixes

argListName :: Var
argListName = Var "!!"

argumentBindings :: [e] -> [(Var, e)]
argumentBindings = zip argumentNames

isImplicitlyGlobal :: Var -> Bool
isImplicitlyGlobal (Var ('#':_)) = True
isImplicitlyGlobal (Var "%") = True
isImplicitlyGlobal _ = False
