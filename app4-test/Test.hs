module Test (main) where

import Books
import Control.Monad.Reader
import Main (main')
import Support
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "using TestAppIO" $ do
    around (withDB ":memory:") $ do
      it "Showing a message when no books are found" $ \db -> do
        expectConsoleCallsIO
          [ PrintLine "Welcome to the Library" (),
            GetStringInput "Search: " "Pri",
            PrintLine "No books found for: Pri" (),
            GetStringInput "Search: " "",
            PrintLine "Bye!" ()
          ]
          (main' db)

      it "User can perform searches and exit" $ \db -> do
        liftIO $ addBook db Book {title = "Pride and Prejudice", author = "Jane Austen"}
        liftIO $ addBook db Book {title = "1984", author = "George Orwell"}
        liftIO $ addBook db Book {title = "Frankenstein", author = "Mary Shelley"}

        expectConsoleCallsIO
          [ PrintLine "Welcome to the Library" (),
            GetStringInput "Search: " "en",
            PrintLine " * Pride and Prejudice, Jane Austen" (),
            PrintLine " * Frankenstein, Mary Shelley" (),
            GetStringInput "Search: " "or",
            PrintLine " * 1984, George Orwell" (),
            GetStringInput "Search: " "",
            PrintLine "Bye!" ()
          ]
          (main' db)

  describe "using TestApp" $ do
    it "Showing a message when no books are found" $ do
      expectConsoleCalls
        (const [])
        [ PrintLine "Welcome to the Library" (),
          GetStringInput "Search: " "Pri",
          PrintLine "No books found for: Pri" (),
          GetStringInput "Search: " "",
          PrintLine "Bye!" ()
        ]
        (main' undefined)

    it "User can perform searches and exit" $ do
      expectConsoleCalls
        ( \q -> case q of
            "en" ->
              [ Book {title = "Pride and Prejudice", author = "Jane Austen"},
                Book {title = "Frankenstein", author = "Mary Shelley"}
              ]
            "or" ->
              [ Book {title = "1984", author = "George Orwell"}
              ]
            _ -> []
        )
        [ PrintLine "Welcome to the Library" (),
          GetStringInput "Search: " "en",
          PrintLine " * Pride and Prejudice, Jane Austen" (),
          PrintLine " * Frankenstein, Mary Shelley" (),
          GetStringInput "Search: " "or",
          PrintLine " * 1984, George Orwell" (),
          GetStringInput "Search: " "",
          PrintLine "Bye!" ()
        ]
        (main' undefined)
