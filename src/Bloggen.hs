module Bloggen
  ( main,
    process,
  )
where

import Bloggen.Convert (convert)
import qualified Bloggen.Html as Html
import qualified Bloggen.Markup as Markup
import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents
      putStrLn (process "User input" content) -- TODO: Too lazy to work
    [input, output] -> do
      content <- readFile input
      exist <- doesFileExist output
      let writeResult = writeFile output $ process input content
      if exist
        then whenIO (confirm input) writeResult
        else writeResult
    _ ->
      putStr
        "Incorrect use!.\nValid uses:\n1)call the program without arguments\
        \to parse the stdin.\n2) call the program with two arguments the input file and the outputfile"

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  when result action

confirm :: String -> IO Bool
confirm name = do
  putStrLn ("Do you want to overwrite the file " <> name <> "? (y/n)")
  line <- getLine
  case line of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm name

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
