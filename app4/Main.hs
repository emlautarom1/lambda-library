module Main where

import Books hiding (findBook)
import Books qualified as Book
import Control.Monad
import System.IO

class (Monad m) => HasConsole m where
  getStringInput :: String -> m String
  printLine :: String -> m ()

class (Monad m) => CanReadBookDB m where
  findBook :: BookDB -> String -> m [Book]

instance HasConsole IO where
  getStringInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

  printLine = putStrLn

instance CanReadBookDB IO where
  findBook = Book.findBook

main :: IO ()
main = do
  withDB
    "./books.db"
    ( \db -> do
        main' db
    )

main' :: (HasConsole m, CanReadBookDB m) => BookDB -> m ()
main' db = do
  printLine "Welcome to the Library"
  loop db

loop :: (HasConsole m, CanReadBookDB m) => BookDB -> m ()
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

printBookList :: (HasConsole m) => [Book] -> m ()
printBookList books =
  forM_ books (\book -> printLine $ " * " <> book.title <> ", " <> book.author)
