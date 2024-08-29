module Books (Book (..), BookDB, openDB, closeDB, withDB, findBook, addBook) where

import Control.Exception (bracket)
import Database.SQLite.Simple qualified as DB

data Book = Book {title :: String, author :: String}
  deriving (Eq, Show)

data BookDB = BookDB
  { findBook :: String -> IO [Book],
    addBook :: Book -> IO (),
    closeDB :: IO ()
  }

openDB :: FilePath -> IO BookDB
openDB file = do
  conn <- DB.open file
  DB.execute_ conn "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY, title TEXT, author TEXT)"
  pure
    BookDB
      { findBook = \query -> do
          map
            ( \(title, author) ->
                Book {title = title, author = author}
            )
            <$> DB.query
              conn
              "SELECT title, author from books WHERE title LIKE concat('%',?,'%') OR author LIKE concat('%',?,'%')"
              (query, query),
        addBook = \book -> do
          DB.execute conn "INSERT INTO books (title, author) VALUES (?, ?)" (book.title, book.author),
        closeDB =
          DB.close conn
      }

withDB :: FilePath -> (BookDB -> IO ()) -> IO ()
withDB file = bracket (openDB file) closeDB
