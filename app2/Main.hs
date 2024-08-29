module Main where

import Books
import Control.Monad (forM_)
import System.IO

data ConsoleHandler = ConsoleHandler
  { getStringInput :: String -> IO String,
    printLine :: String -> IO ()
  }

consoleHandler :: ConsoleHandler
consoleHandler =
  ConsoleHandler
    { getStringInput = \prompt -> do
        putStr prompt
        hFlush stdout
        getLine,
      printLine = putStrLn
    }

main :: IO ()
main =
  withDB "./books.db" (main' consoleHandler)

main' :: ConsoleHandler -> BookDB -> IO ()
main' c db = do
  c.printLine "Welcome to the Library"
  loop c db

loop :: ConsoleHandler -> BookDB -> IO ()
loop c db = do
  query <- c.getStringInput "Search: "
  case query of
    "" ->
      c.printLine "Bye!"
    _ -> do
      books <- findBook db query
      if null books
        then
          c.printLine $ "No books found for: " <> query
        else
          printBookList c books
      loop c db

printBookList :: ConsoleHandler -> [Book] -> IO ()
printBookList c books =
  forM_ books (\book -> c.printLine $ " * " <> book.title <> ", " <> book.author)
