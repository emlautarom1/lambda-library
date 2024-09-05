# A Haskell tour on side-effects: Part 2, Console Handler

In this alternative we are going to change the implementation of our program so we can replace the functions to interact with the console in tests. This way we won't need to mess with STDIN/STDOUT as before.

We will define the functions to interact with the console in a `ConsoleHandler`.

```haskell
data ConsoleHandler = ConsoleHandler
  { getStringInput :: String -> IO String,
    printLine :: String -> IO ()
  }
```

For the production application we will have a function that provides the _real_ implementation of such functions.

```haskell
consoleHandler :: ConsoleHandler
consoleHandler =
  ConsoleHandler
    { getStringInput = \prompt -> do
        putStr prompt
        hFlush stdout
        getLine,
      printLine = putStrLn
    }
```

Our `main'` function should continue to receive the `BookDB` as before and now an additional `ConsoleHandler` argument which will be initialized in the `main`.

```haskell
main :: IO ()
main =
  withDB "./books.db" (main' consoleHandler)

main' :: ConsoleHandler -> BookDB -> IO ()
main' c db = do
  c.printLine "Welcome to the Library"
  loop c db

loop :: ConsoleHandler -> BookDB -> IO ()
loop = _
```

Thanks to [`OverloadedRecordDot`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html) we write `c.printLine "Welcome to the Library"` instead of `printLine c "Welcome to the Library"` which plays particularly nice in this case since the functions to execute are values defined in the `ConsoleHandler` itself.

The `loop` implementation needs to be adapted as well to use a `ConsoleHandler`.

```haskell
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
```

All these changes are straight-forward, but some might think that are a bit verbose. All helper functions will need to receive the handler explicitly. In our case the `printBookList` will look as follows:

```haskell
printBookList :: ConsoleHandler -> [Book] -> IO ()
printBookList c books =
  forM_ books (\book -> c.printLine $ " * " <> book.title <> ", " <> book.author)
```

This version of the program is essentially the same as the previous one. The main different is how we are able to write the tests.

For example we could choose to have a mock `ConsoleHandler` that records all the calls to `printLine` in an `IORef [String]`, and knows what to return for each `getStringInput`.

```haskell
mockConsoleHandler :: [String] -> IO (IORef [String], ConsoleHandler)
--                    ↑                     ↑
-- user inputs ───────┘                     │
-- output lines ─────────────────────────────┘
```

With a `mockConsoleHandler` we can write the same spec as before but without messing with STDIN and STDOUT.

```haskell
main :: IO ()
main = hspec $ do
  around (withDB ":memory:") $ do
    it "Showing a message when no books are found" $ \db -> do
      (output, c) <- mockConsoleHandler ["Pri", ""]

      main' c db

      readIORef output
        >>= shouldBe
          [ "Welcome to the Library",
            "No books found for: Pri",
            "Bye!"
          ]
```

But the `mockConsoleHandler` does not allow us to assert exactly when the calls to `printLine` happen with respect `getStringInput`. This is not ideal.

This is a limitation of the implementation choice of the _mock handler_. We can choose to implement something more expressive for this case. Sometimes simpler mocks are enough, but not this time.

One option is to create a mock handler that will allow only a strict order of calls, and that will know what to return for each. Like having a tape of the exact interaction we expect and using that tape for the test.

```haskell
data ConsoleTapeEntry = GetStringInput String String | PrintLine String ()
  deriving (Eq, Show)

expectConsoleCalls :: [ConsoleTapeEntry] -> (ConsoleHandler -> IO a) -> IO a
```

With the above declaration we can now rewrite the specs with the specific order of calls to expect, and again, without messing with STDIN and STDOUT.

```haskell
main :: IO ()
main = hspec $ do
  around (withDB ":memory:") $ do
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
```

To reiterate we have a wide range of options on how to implement the mock handler for our tests. The one we are using in the last example can even be generated dynamically by the compiler on the fly, but if not, is very mechanical.

Using handlers to decouple the implementation has the benefit of using very simple concepts of the language. The downside is that wiring the handlers on all the functions might feel tedious. You can read more about the [handler pattern at jaspervdj.be](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).

The introduction of the handler pattern allow us to restrict other side-effects from happening. Let's see how to achieve that next.

> [!note] You can find a working copy of this code in `app2` and `app2-test` in [github:bcardiff/lambda-library](https://github.com/bcardiff/lambda-library)