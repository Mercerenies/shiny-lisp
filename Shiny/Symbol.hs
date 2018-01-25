{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shiny.Symbol(SymbolTable, Symbols(..), throwS, catchS, runSymbols, runSymbols', emptyState,
                    hasSymbol, getSymbol, setSymbol, defSymbol, undefSymbol,
                    getSymbolOrDefault, getSymbolMaybe, getSymbolDefining,
                    globalDefSymbol, setOrDefSymbol,
                    pushStack, popStack, callStackDepth, forceReset) where

import Shiny.Vars
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map(Map)
import qualified Data.Map as Map

type SymbolTable a = Map Var a

newtype Symbols e a = Symbols (ExceptT String (StateT [SymbolTable e] IO) a)
    deriving (Functor, Applicative, Monad, MonadState [SymbolTable e], MonadIO)

throwS :: String -> Symbols e a
throwS = Symbols . throwE

catchS :: Symbols e a -> (String -> Symbols e a) -> Symbols e a
catchS (Symbols x) f = Symbols $ catchE x f'
    where f' y = let ~(Symbols y') = f y in y'

runSymbols :: [SymbolTable e] -> Symbols e a -> IO (Either String a)
runSymbols st (Symbols x) = evalStateT (runExceptT x) st

runSymbols' :: [SymbolTable e] -> Symbols e a -> IO (Either String a, [SymbolTable e])
runSymbols' st (Symbols x) = runStateT (runExceptT x) st

emptyState :: [SymbolTable e]
emptyState = [Map.empty]

hasSymbol :: Var -> Symbols e Bool
hasSymbol v = catchS inner (const $ pure False)
    where inner = do
            void $ getSymbol v
            return True

getSymbol :: Var -> Symbols e e
getSymbol v = do
  xs <- get
  case msum $ map (Map.lookup v) xs of
    Nothing -> throwS $ "variable " ++ getVar v ++ " does not exist"
    Just y -> pure y

setSymbol :: Var -> e -> Symbols e ()
setSymbol v a = do
  ys <- get
  case modified ys of
    Nothing -> throwS $ "variable " ++ getVar v ++ " does not exist"
    Just xs -> put xs
    where modified [] = Nothing
          modified (y:ys) = case Map.lookup v y of
                              Nothing -> (y :) <$> modified ys
                              Just _ -> Just (Map.adjust (const a) v y : ys)

defSymbol :: Var -> e -> Symbols e ()
defSymbol v y = do
  ys <- get
  case ys of
    [] -> throwS "call stack is empty"
    (x:xs) -> put $ Map.insert v y x : xs

undefSymbol :: Var -> Symbols e ()
undefSymbol v = get >>= helper >>= put
    where helper [] = throwS "variable does not exist"
          helper (x:xs)
              | Map.member v x = pure $ Map.delete v x : xs
              | otherwise = (x :) <$> helper xs

getSymbolOrDefault :: Var -> e -> Symbols e e
getSymbolOrDefault v def = do
  exists <- hasSymbol v
  if exists then
      getSymbol v
  else
      pure def

getSymbolMaybe :: Var -> Symbols e (Maybe e)
getSymbolMaybe v = do
  exists <- hasSymbol v
  if exists then
      Just <$> getSymbol v
  else
      pure Nothing

getSymbolDefining :: Var -> e -> Symbols e e
getSymbolDefining v def = do
  val <- getSymbolOrDefault v def
  setOrDefSymbol v val
  pure val

globalDefSymbol :: Var -> e -> Symbols e ()
globalDefSymbol v y = do
  ys <- get
  case ys of
    [] -> throwS "call stack is empty"
    xs -> put $ init xs ++ [Map.insert v y $ last xs]

setOrDefSymbol :: Var -> e -> Symbols e ()
setOrDefSymbol v a = do
  exists <- hasSymbol v
  case () of
    _ | exists               -> setSymbol       v a
      | isImplicitlyGlobal v -> globalDefSymbol v a
      | otherwise            -> defSymbol       v a

pushStack :: Symbols e ()
pushStack = modify (Map.empty :)

popStack :: Symbols e (SymbolTable e)
popStack = get >>= \ys -> case ys of
                            [] -> throwS "call stack is empty"
                            (x:xs) -> x <$ put xs

-- TODO Integral?
callStackDepth :: Symbols e Int
callStackDepth = Symbols $ gets length

-- Be VERY careful with this; it can break call stacks very easily
forceReset :: [SymbolTable e] -> Symbols e ()
forceReset = Symbols . lift . put
