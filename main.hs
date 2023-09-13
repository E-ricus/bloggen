{-# LANGUAGE LambdaCase #-}

import Convert
import Html
import Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [] -> getContents >>= \content -> putStrLn (process "User input" content) -- TODO: Too lazy to work
    [input, output] ->
      doesFileExist output >>= \case
        True ->
          confirm output >>= \case
            True -> readAndWrite input output
            False -> putStr "Bye then."
        False -> readAndWrite input output
    _ ->
      putStr
        "Incorrect use!.\nValid uses:\n1)call the program without arguments\
        \to parse the stdin.\n2) call the program with two arguments the input file and the outputfile"

readAndWrite :: FilePath -> FilePath -> IO ()
readAndWrite input output = readFile input >>= \content -> writeFile output $ process input content

confirm :: String -> IO Bool
confirm name =
  putStrLn ("Do you want to overwrite the file " <> name <> "? (y/n)")
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Invalid response. use y or n"
          *> confirm name

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
