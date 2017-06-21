
module Shiny.Runner(evalExpr, evalExprs, evalExpr', evalExprs',
                    readAndEval, readAndEval', readEvalPrint,
                    evalFile, evalFileThenPrint) where

import Prelude hiding (lookup)
import Data.Map(lookup)
import Control.Monad.IO.Class
import Shiny.Standard
import Shiny.Eval
import Shiny.Vars
import Shiny.Parser
import Shiny.Structure
import Shiny.Symbol
import System.Environment

evalExpr :: Expr -> IO (Either String Expr, [SymbolTable Expr])
evalExpr e = runSymbols' standardState $ evaluate e

evalExpr' :: Expr -> IO (Either String Expr)
evalExpr' = fmap fst . evalExpr

evalExprs :: [Expr] -> IO (Either String Expr, [SymbolTable Expr])
evalExprs e = runSymbols' standardState $ do
                liftIO getArgs >>= (bindArgs . map toExpr)
                evalSeq e

evalExprs' :: [Expr] -> IO (Either String Expr)
evalExprs' = fmap fst . evalExprs

readAndEval :: String -> IO (Either String Expr, [SymbolTable Expr])
readAndEval s = case readExpr s of
                  Left x -> pure $ (Left x, [])
                  Right y -> evalExprs y

readAndEval' :: String -> IO (Either String Expr)
readAndEval' = fmap fst . readAndEval

readEvalPrint :: String -> IO ()
readEvalPrint s = do
  e <- readAndEval' s
  case e of
    Left x -> putStrLn $ x
    Right y -> putStrLn $ printable y

evalFile :: FilePath -> IO [SymbolTable Expr]
evalFile x = readFile x >>= (fmap snd . readAndEval)

evalFileThenPrint :: FilePath -> IO ()
evalFileThenPrint x = do
  sym <- evalFile x
  case sym of
    [m] -> case lookup (Var "%") m of
             Just y -> putStrLn $ userPrint y
             Nothing -> pure ()
    _ -> pure ()
