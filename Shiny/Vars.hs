
module Shiny.Vars(Var(..), argumentNames, argumentBindings, isImplicitlyGlobal) where

newtype Var = Var { getVar :: String }
    deriving (Show, Read, Eq, Ord)

argumentNames :: [Var]
argumentNames = let base = ["x", "y", "z"]
                    layer :: Integer -> [String]
                    layer n = map (++ show n) base
                in map Var $ base ++ concatMap layer [0..]

argumentBindings :: [e] -> [(Var, e)]
argumentBindings = zip argumentNames

isImplicitlyGlobal :: Var -> Bool
isImplicitlyGlobal (Var ('#':_)) = True
isImplicitlyGlobal (Var "%") = True
isImplicitlyGlobal _ = False
