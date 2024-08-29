module Test (main) where

import Books
import Main (main', readOnlyBookDBHandler)
import Support
import Test.Hspec

main :: IO ()
main = hspec $ do
  around (withDB ":memory:") $ do
    it "Showing a message when no books are found" $ \db -> do
      rodbh <- readOnlyBookDBHandler db

      expectConsoleCalls
        [ PrintLine "Welcome to the Library" (),
          GetStringInput "Search: " "Pri",
          PrintLine "No books found for: Pri" (),
          GetStringInput "Search: " "",
          PrintLine "Bye!" ()
        ]
        (\c -> main' c rodbh)

    it "User can perform searches and exit" $ \db -> do
      addBook db Book {title = "Pride and Prejudice", author = "Jane Austen"}
      addBook db Book {title = "1984", author = "George Orwell"}
      addBook db Book {title = "Frankenstein", author = "Mary Shelley"}

      rodbh <- readOnlyBookDBHandler db

      expectConsoleCalls
        [ PrintLine "Welcome to the Library" (),
          GetStringInput "Search: " "en",
          PrintLine " * Pride and Prejudice, Jane Austen" (),
          PrintLine " * Frankenstein, Mary Shelley" (),
          GetStringInput "Search: " "or",
          PrintLine " * 1984, George Orwell" (),
          GetStringInput "Search: " "",
          PrintLine "Bye!" ()
        ]
        (\c -> main' c rodbh)
