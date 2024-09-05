# A Haskell tour on side-effects: Part 1, Introduction

We are going to develop a small application for users to search books within a library.

The application will open a connection to a database, wait for user input, perform a search
and repeat, until the user input is empty and exit.

A session in this application could look as follow:

```
Welcome to the Library
Search: qwerty
No books found for: qwerty
Search: en
 * Pride and Prejudice, Jane Austen
 * Frankenstein, Mary Shelley
Search: or
 * 1984, George Orwell
Search:
Bye!
```

We are going to focus on how to perform the side-effects. How can we structure the code
and how we can write tests. We are going to go through different implementations of this
same program to have a better understanding of the alternatives.

We want to focus on how each alternative feel and looks like. This tour is not aim to propose that either solutions is better than other but to experience the difference between them.

We will assume the interation with the database is already in place. In particular we
will have an implementation with SQLite, but that will not show up through the tour.

The `Books` module will expose functions to open and close a DB, and to add and find books.

```haskell
module Books (Book (..), BookDB, openDB, closeDB, findBook, addBook) where

data Book = Book {title :: String, author :: String}
  deriving (Eq, Show)

data BookDB = ...

openDB :: FilePath -> IO BookDB
closeDB :: BookDB -> IO ()
findBook :: BookDB -> String -> IO [Book]
addBook :: BookDB -> Book -> IO ()
withDB :: FilePath -> (BookDB -> IO ()) -> IO ()
```

The `withDB` function will open and close a DB using [`bracket`](https://hackage.haskell.org/package/base-4.17.2.1/docs/Control-Exception.html#v:bracket).

To be able to interact with the console for getting user input, printing strings and list of books
we will need a couple of functions. Their actual implementation doesn't matter much, similar 
to the `Book` module. The important thing to notice is that they return `IO a`.

```haskell
getStringInput :: String -> IO String
printLine :: String -> IO ()
printBookList :: [Book] -> IO ()
```

Once those are defined the actual program can be defined as follows.

```haskell
main :: IO ()
main =
  withDB
    "./books.db"
    ( \db -> do
        printLine "Welcome to the Library"
        loop db
    )

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
```

The `main` will open a fixed DB, present itself and start the loop of asking user input and reacting to it.

And this is a fully working example of what we want. But if we aim to write some tests we face a couple of challenges.

Althought the `loop` receives a `BookDB`, we can't change the DB for the whole `main`. We make `main` only setup the database connection and move the whole application logic into a `main'` function. This way, we will be able to write tests for `main'` and be in total control of which DB to use for each test.

```haskell
main :: IO ()
main =
  withDB "./books.db" main'

main' :: BookDB -> IO ()
main' db = do
  printLine "Welcome to the Library"
  loop db
```

In order to write tests we will need to simulate user input and write expectation of the generated output.
For that we need to replace STDIN and STDOUT of the current process. This is doable. The OS allow us to do that.

We can have a `runWithStdinAndCapture :: [String] -> IO () -> IO [String]` that will execute the given IO with the given user input, and return the output as a `[String]`. Yet, there are some drawbacks of this approach:

* Changing the STDIN and STDOUT of the process could mess with the test framework (like hidding errors).
* It's harder to assert in which order the user inputs are consumed and output generated. This can be fixed but requires a more complex `runWithStdinAndCapture`.

Nevertheless, let's try to write a couple of specs:

* Showing a message when no books are found
* User can perform searches and exit

```haskell
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
      db.addBook Book {title = "Pride and Prejudice", author = "Jane Austen"}
      db.addBook Book {title = "1984", author = "George Orwell"}
      db.addBook Book {title = "Frankenstein", author = "Mary Shelley"}

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
```

Although it works we have something odd in the assertions.

```haskell
      output
        `shouldBe` [ "Welcome to the Library",
                      "Search: No books found for: Pri",
                      "Search: Bye!"
                    ]
```

The `Search: No books found for: Pri` is different from what we see in the console:

```
Search: Pri
No books found for: Pri
```

That's because when the user inputs "Pri" they ten press `<Enter>` generating a new line in the console. But this new line is not present in the STDOUT.

A more precise test would be one that allow us to express in which order the user inputs and the output happens. Something like:

```haskell
      runExpecting (main' db)
        [ Output "Welcome to the Library"
        , Output "Search: "
        , Input "en"
        , Output " * Pride and Prejudice, Jane Austen"
        , Output " * Frankenstein, Mary Shelley"
        , Output "Search: "
        , Input "or"
        , Output " * 1984, George Orwell"
        , Output "Search: "
        , Input ""
        , Output "Bye!"
        ]
```

Achieving that requires some fine control of STDIN and STDOUT. Let's move on and restructure a bit our application
to allow simpler ways of testing it and see what other gains we can get.

> [!note] You can find a working copy of this code in `app1` and `app1-test` in [github:bcardiff/lambda-library](https://github.com/bcardiff/lambda-library)