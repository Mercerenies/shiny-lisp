
module Shiny.Vars(Var(..),
                  argumentNames, argListName, argumentBindings,
                  reArgumentNames, reArgListName, reArgumentBindings,
                  reFullName,
                  implicitName, delimiterName, dotDelimiterName, stackName,
                  loopContinueName,
                  isImplicitlyGlobal) where

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

reArgumentNames :: [Var]
reArgumentNames = map (\(Var x) -> Var $ 'r' : x) argumentNames

reArgListName :: Var
reArgListName = Var "r!"

reArgumentBindings :: [e] -> [(Var, e)]
reArgumentBindings = zip reArgumentNames

reFullName :: Var
reFullName = Var "rr"

implicitName :: Var
implicitName = Var "%"

delimiterName :: Var
delimiterName = Var "#,"

dotDelimiterName :: Var
dotDelimiterName = Var "#,,"

stackName :: Var
stackName = Var "#v"

loopContinueName :: Var
loopContinueName = Var "&&l"

isImplicitlyGlobal :: Var -> Bool
isImplicitlyGlobal (Var ('#':_)) = True
isImplicitlyGlobal (Var "%") = True
isImplicitlyGlobal _ = False
