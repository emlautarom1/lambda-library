# A Haskell tour on side-effects: Part 4, Type Classes

In the previous one we used the handler pattern and a custom `Task` monad to stop other side-effects from happening. In this alternative we are going to use type classes as a language feature to provide the same guarantees.

Let's start by defining the side-effects for a console in a new type class `HasConsole`. We want this type class to be used as a `Monad`, hence we enforce that by the `Monad m` constraint.

```haskell
class (Monad m) => HasConsole m where
  getStringInput :: String -> m String
  printLine :: String -> m ()
```

`HasConsole` allow us to write programs using a generic monad. By doing so we forbid any other side-effect. See, the `m` monad of the following `greeter` function only has the console functions.

```haskell
greeter :: (HasConsole m) => m ()
greeter = do
  name <- getStringInput "name: "
  printLine $ "Hi " <> name <> "!"
```

If we were to put a `MonadIO m` constraint, we would allow arbitrary side-effects via `liftIO` function. But we are not doing that.

The `greeter` function can be used as an `IO` if we provide a `HasConsole IO` instance.

```haskell
instance HasConsole IO where
  getStringInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

  printLine = putStrLn
```

And for tests we are going to provide another instance in a bit. But first let's write the full library search program.

If we apply the same recipe of putting the functions that we want in a type class for a generic monad we will end up with `CanReadBookDB`.

```haskell
class (Monad m) => CanReadBookDB m where
  findBook :: BookDB -> String -> m [Book]
```

For now we are going to stick with it, but later we are going to improve things. The reason for the eventual change is that we can't do much with a `BookDB` if we are not in a `IO`, yet we want a generic monad for `CanReadBookDB`.

An implementation of `CanReadBookDB` for `IO` that will be used in the real program is just a forward to the existing `Book.findBook` function.

```haskell
instance CanReadBookDB IO where
  findBook = Book.findBook
```

Rewriting the `main` and other functions shows a bit of cleaner code with respect the previous alternative. But requires familiarity with the type classes. What before was a handler value it is now a constraint. The implementation of all the functions are generic. It is `main` who use `main'` instantiated in the `IO` monad.

```haskell
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
```

For the tests we have multiple alternatives. We can have the specs instantiate the `main'` in a monad that has nothing to do with `IO`, or we can allow the specs to use `IO`. Avoding `IO` for tests doesn't bring any benefit, but is a nice excercise. In either case we are going to need some state.

In the previous alternative the `ConsoleHandler` had some `IORef` as a state, but in a type class, where can that state live?

If we want to avoid `IO` as much as possible in the test we are going to need something like `StateT` from `mtl`. Since we can't perform `IO` we are going to cary as state the list of expected console calls `[ConsoleTapeEntry]` and a mock implementation of `findBook :: String -> [Books.Book]`.

```haskell
data Result value
  = Ok value
  | Err String
  deriving (Show, Eq)

instance Monad Result where
  -- ...

instance MonadFail Result where
  -- ...

newtype TestApp a = TestApp (StateT (String -> [Books.Book], [ConsoleTapeEntry]) Result a)
  deriving (Functor, Applicative, Monad, MonadFail)

instance HasConsole TestApp where
  -- ...

instance CanReadBookDB TestApp where
  -- ...

expectConsoleCalls :: (String -> [Books.Book]) -> [ConsoleTapeEntry] -> TestApp a -> IO a
expectConsoleCalls find tape (TestApp f) = do
  case runStateT f (find, tape) of
    Ok (v, (_, rest)) -> do
      unless
        (null rest)
        (fail $ "Expected more calls to happen " <> show rest)

      pure v
    Err err -> fail err

```

Since our helper functions for test use `fail` we need to implement also `MonadFail`, yet this was not a requirement for `main`.
Once the above functions are implemeted mostly by using `get` and `put` to access the `(String -> [Books.Book], [ConsoleTapeEntry])` state from `StateT` we can implement the tests.

```haskell
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
```

You might have noticied the odd `undefined`. Since tests using `TestApp` will not perform `IO` we don't need an actual `BookDB` value, hence using `undefined` is fine. In the next alternative we are gonig to improve this.

We also mentioned that for tests we could use `IO`. To keep the `IORef` as we did for the handlers we are going to need `ReaderT` from `mtl`.

```haskell
newtype TestAppIO a = TestAppIO (ReaderT (IORef [ConsoleTapeEntry]) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
```

The full implementation of `HasConsole TestAppIO` is not that different from the handler pattern one. We use `ask` to get the `IORef` state and call `liftIO` to do the `IO` as needed.

```haskell
popTapeIO :: String -> TestAppIO ConsoleTapeEntry
popTapeIO msg = do
  tape <- TestAppIO ask
  current <- liftIO $ readIORef tape
  when
    (null current)
    (fail $ "Unexpected call " <> msg)
  liftIO $ writeIORef tape (tail current)
  pure (head current)

instance HasConsole TestAppIO where
  getStringInput arg = do
    entry <- popTapeIO $ "GetStringInput " <> show arg
    case entry of
      GetStringInput arg' res' | arg' == arg -> pure res'
      _ -> fail $ "Expected call matching " <> show entry <> " got GetStringInput " <> show arg

  printLine arg = do
    entry <- popTapeIO $ "PrintLine " <> show arg
    case entry of
      PrintLine arg' res' | arg' == arg -> pure res'
      _ -> fail $ "Expected call matching " <> show entry <> " got PrintLine " <> show arg
```

The `CanReadBookDB TestAppIO` needs to be implemeted for test also because the instance in our `Main` module as bound to `IO`.

```haskell
instance CanReadBookDB TestAppIO where
  findBook db q = liftIO $ Books.findBook db q
```

We could have reused it if it would have been defined based on `MonadIO`.

```haskell
instance (Monad m, MonadIO m) => CanReadBookDB m where
  findBook db q = liftIO $ Book.findBook db q
```

Finally, the specs looks very similar to the handler pattern. The main difference, as with `main'` is that we no longer need to pass an explict `ConsoleHandler`.

```haskell
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
```

Next we can iterate on how we handle the state of our application.

> [!note] You can find a working copy of this code in `app4` and `app4-test` in [github:bcardiff/lambda-library](https://github.com/bcardiff/lambda-library)