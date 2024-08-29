module Main where

import App
import Books hiding (findBook)
import Books qualified as Book
import Control.Monad.Reader
import System.IO

class (Monad m) => HasConsole m where
  getStringInput :: String -> m String
  printLine :: String -> m ()

class (Monad m) => HasReadOnlyBookDB m where
  findBook :: String -> m [Book]

instance HasConsole App where
  getStringInput prompt = liftIO $ do
    putStr prompt
    hFlush stdout
    getLine

  printLine = liftIO . putStrLn

instance HasReadOnlyBookDB App where
  findBook q = do
    db' <- asks db
    liftIO $ Book.findBook db' q

main :: IO ()
main = do
  withDB
    "./books.db"
    ( \db -> do
        runApp Env {db = db} main'
    )

main' :: (HasConsole m, HasReadOnlyBookDB m) => m ()
main' = do
  printLine "Welcome to the Library"
  loop

loop :: (HasConsole m, HasReadOnlyBookDB m) => m ()
loop = do
  query <- getStringInput "Search: "
  case query of
    "" ->
      printLine "Bye!"
    _ -> do
      books <- findBook query
      if null books
        then
          printLine $ "No books found for: " <> query
        else
          printBookList books
      loop

printBookList :: (HasConsole m) => [Book] -> m ()
printBookList books =
  forM_ books (\book -> printLine $ " * " <> book.title <> ", " <> book.author)
