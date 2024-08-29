module Main where

import Books hiding (findBook)
import Books qualified as Book
import Control.Monad (forM_)
import System.IO
import Task

data ConsoleHandler = ConsoleHandler
  { getStringInput :: String -> Task String,
    printLine :: String -> Task ()
  }

consoleHandler :: IO ConsoleHandler
consoleHandler = do
  h <- doAnythingHandler
  pure
    ConsoleHandler
      { getStringInput = \prompt -> doAnything h $ do
          putStr prompt
          hFlush stdout
          getLine,
        printLine = doAnything h . putStrLn
      }

newtype ReadOnlyBookDBHandler = ReadOnlyBookDBHandler
  { findBook :: String -> Task [Book]
  }

readOnlyBookDBHandler :: BookDB -> IO ReadOnlyBookDBHandler
readOnlyBookDBHandler db = do
  h <- doAnythingHandler
  pure
    ReadOnlyBookDBHandler
      { findBook = doAnything h . Book.findBook db
      }

main :: IO ()
main = do
  withDB
    "./books.db"
    ( \db -> do
        c <- consoleHandler
        db' <- readOnlyBookDBHandler db
        Task.perform (main' c db')
    )

main' :: ConsoleHandler -> ReadOnlyBookDBHandler -> Task ()
main' c db = do
  c.printLine "Welcome to the Library"
  loop c db

loop :: ConsoleHandler -> ReadOnlyBookDBHandler -> Task ()
loop c db = do
  query <- c.getStringInput "Search: "
  case query of
    "" ->
      c.printLine "Bye!"
    _ -> do
      books <- db.findBook query
      if null books
        then
          c.printLine $ "No books found for: " <> query
        else
          printBookList c books
      loop c db

printBookList :: ConsoleHandler -> [Book] -> Task ()
printBookList c books =
  forM_ books (\book -> c.printLine $ " * " <> book.title <> ", " <> book.author)
