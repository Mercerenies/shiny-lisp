
import Shiny.Runner
import Shiny.Eval
import Shiny.Structure
import Shiny.Symbol
import Shiny.Standard
import Shiny.Parser
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

showHelp :: IO ()
showHelp = do
  putStrLn "Usage:"
  putStrLn "  shiny - Open a REPL"
  putStrLn "  shiny <filename> <args...> - Execute the file, passing the given command line arguments"
  putStrLn "  shiny --help - Show the help screen"

doREPL :: IO ()
doREPL = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  void . runSymbols standardState . forever $ catchS operation (liftIO . putStrLn)
      where operation = do
              liftIO $ putStr "> "
              x <- liftIO $ getLine
              e <- Symbols . ExceptT . pure $ readExpr x
              res <- evalSeq e
              liftIO $ putStrLn (printable res)

doFile :: FilePath -> [String] -> IO ()
doFile file args = withArgs args (evalFileThenPrint file)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    [] -> doREPL
    (x:xs) -> doFile x xs
