module Test (main) where

import Books
import Data.IORef (readIORef)
import Main (main')
import Support
import Test.Hspec

main :: IO ()
main = hspec $ do
  around (withDB ":memory:") $ do
    describe "using mockConsoleHandler" $ do
      it "Showing a message when no books are found" $ \db -> do
        (output, c) <- mockConsoleHandler ["Pri", ""]

        main' c db

        readIORef output
          >>= shouldBe
            [ "Welcome to the Library",
              "No books found for: Pri",
              "Bye!"
            ]

      it "User can perform searches and exit" $ \db -> do
        addBook db Book {title = "Pride and Prejudice", author = "Jane Austen"}
        addBook db Book {title = "1984", author = "George Orwell"}
        addBook db Book {title = "Frankenstein", author = "Mary Shelley"}

        (output, c) <- mockConsoleHandler ["en", "or", ""]

        main' c db

        readIORef output
          >>= shouldBe
            [ "Welcome to the Library",
              " * Pride and Prejudice, Jane Austen",
              " * Frankenstein, Mary Shelley",
              " * 1984, George Orwell",
              "Bye!"
            ]

    describe "using expectConsoleCalls" $ do
      it "Showing a message when no books are found" $ \db -> do
        expectConsoleCalls
          [ PrintLine "Welcome to the Library" (),
            GetStringInput "Search: " "Pri",
            PrintLine "No books found for: Pri" (),
            GetStringInput "Search: " "",
            PrintLine "Bye!" ()
          ]
          (\c -> main' c db)

      it "User can perform searches and exit" $ \db -> do
        addBook db Book {title = "Pride and Prejudice", author = "Jane Austen"}
        addBook db Book {title = "1984", author = "George Orwell"}
        addBook db Book {title = "Frankenstein", author = "Mary Shelley"}

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
          (\c -> main' c db)
