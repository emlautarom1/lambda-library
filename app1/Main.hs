module Main where

import Books
import Control.Monad (forM_)
import System.IO

main :: IO ()
main =
  withDB "./books.db" main'

main' :: BookDB -> IO ()
main' db = do
  printLine "Welcome to the Library"
  loop db

loop :: BookDB -> IO ()
loop db = do
  query <- getStringInput "Search: "
  case query of
    "" ->
      printLine "Bye!"
    _ -> do
      books <- findBook db query
      if null books
        then
          printLine $ "No books found for: " <> query
        else
          printBookList books
      loop db

getStringInput :: String -> IO String
getStringInput prompt = do
  putStr prompt
  hFlush stdout
  getLine

printLine :: String -> IO ()
printLine = putStrLn

printBookList :: [Book] -> IO ()
printBookList books =
  forM_ books (\book -> printLine $ " * " <> book.title <> ", " <> book.author)
