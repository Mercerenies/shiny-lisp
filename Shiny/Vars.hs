
module Shiny.Vars(Var(..),
                  argumentNames, argListName, argumentBindings,
                  implicitName, delimiterName, dotDelimiterName,
                  isImplicitlyGlobal) where

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

implicitName :: Var
implicitName = Var "%"

delimiterName :: Var
delimiterName = Var "#,"

dotDelimiterName :: Var
dotDelimiterName = Var "#,,"

isImplicitlyGlobal :: Var -> Bool
isImplicitlyGlobal (Var ('#':_)) = True
isImplicitlyGlobal (Var "%") = True
isImplicitlyGlobal _ = False
