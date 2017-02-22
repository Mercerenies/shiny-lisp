
import Shiny.Runner
import System.Environment
import System.IO
import Control.Monad

showHelp :: IO ()
showHelp = do
  putStrLn "Usage:"
  putStrLn "  shiny - Open a REPL"
  putStrLn "  shiny <filenames...> - Execute the files in sequence (with separate environments)"
  putStrLn "  shiny --help - Show the help screen"

doREPL :: IO ()
doREPL = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  forever $ do
           putStr "> "
           x <- getLine
           readEvalPrint x

doFiles :: [FilePath] -> IO ()
doFiles = mapM_ evalFileThenPrint

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    [] -> doREPL
    xs -> doFiles xs
