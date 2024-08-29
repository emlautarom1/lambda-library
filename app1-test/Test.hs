module Test (main) where

import Books
import Main (main')
import Support
import Test.Hspec

main :: IO ()
main = hspec $ do
  around (withDB ":memory:") $ do
    it "Showing a message when no books are found" $ \db -> do
      output <-
        runWithStdinAndCapture ["Pri", ""] $
          main' db

      output
        `shouldBe` [ "Welcome to the Library",
                     "Search: No books found for: Pri",
                     "Search: Bye!"
                   ]

    it "User can perform searches and exit" $ \db -> do
      addBook db Book {title = "Pride and Prejudice", author = "Jane Austen"}
      addBook db Book {title = "1984", author = "George Orwell"}
      addBook db Book {title = "Frankenstein", author = "Mary Shelley"}

      output <-
        runWithStdinAndCapture ["en", "or", ""] $
          main' db

      output
        `shouldBe` [ "Welcome to the Library",
                     "Search:  * Pride and Prejudice, Jane Austen",
                     " * Frankenstein, Mary Shelley",
                     "Search:  * 1984, George Orwell",
                     "Search: Bye!"
                   ]
