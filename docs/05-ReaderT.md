# A Haskell tour on side-effects: Part 5, ReaderT design pattern

In this alternative we are going to iterate a bit on what we did before. We are going to keep our functions writen in terms of a generic monad which allows switching the actual implementation for tests. But we are going to move the `BookDB` value out of the function argument into an implicit state. We are going to do so by implementing the [ReaderT design pattern](https://tech.fpcomplete.com/blog/2017/06/readert-design-pattern/).

The `HasConsole` type class remains unchanged.

```haskell
class (Monad m) => HasConsole m where
  getStringInput :: String -> m String
  printLine :: String -> m ()
```

For performing the book search we are going to define a `HasReadOnlyBookDB` monad with a `findBook` that takes only the search query and returns a (monadic) list of books as results.

```haskell
class (Monad m) => HasReadOnlyBookDB m where
  findBook :: String -> m [Book]
```

Where is the `BookDB`? It will appear soon. Before looking into it, we can implement the rest functions.
The `main'` and `loop` function takes no argumetns since the `BookDB` is going to be implicit now. The implemention is mostly like the previous alternative.

```haskell
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
```

Where things differ from just using type classes and this alternative is that the monad that we are going to use for the production code is no longer `IO` but something else. It's going to be a `ReaderT` over and `Env` that will keep the implicit state (the `BookDB`) and the `IO` monad.

We can choose to use `ReaderT` directly but I prefer a different type to track better where is used, but also be able to restrict whether we want `MonadIO` or `MonadFail` for example.

```haskell
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module App (Env (..), App, runApp) where

import Books
import Control.Monad.Reader

newtype Env = Env {db :: BookDB}

newtype App a = App (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env (App f) =
  runReaderT f env
```

Thanks to `GeneralisedNewtypeDeriving` we can derive `Monad` and `MonadIO` from the wrapped `ReaderT`.

The exported `runApp` will effectively run the `App a` on a given `Env` where the connection is specified. We can now adapt the `main` function to initialize the `Env` to run `main'`.

```haskell
main :: IO ()
main = do
  withDB
    "./books.db"
    ( \db -> do
        runApp Env {db = db} main'
    )
```

We still need to implement `HasConsole App` and `HasReadOnlyBookDB App`. The `asks` from `MonadReader` function gives us a convenient way to get specific values from the `Env`. The `HasConsole App` instance does not need access to the `Env` actually, but there is no need to do something more generic based on `MonadIO`.

```haskell
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
```

A nice aspect of this alternative is that the tests don't differ much from the production code. We can also use `ReaderT` design pattern. We actually used it already in the previous alternative, but now with `Env` we can be a bit more tidy, or at least call it by this name.

```haskell
data Env = Env {db :: BookDB, tape :: IORef [ConsoleTapeEntry]}

newtype TestApp a = TestApp (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadFail)

runTestApp :: Env -> TestApp a -> IO a
runTestApp env (TestApp f) =
  runReaderT f env
```

The `Env` for `TestApp` will have the `IORef` of the expected `HasConsole` functions to be called and the implicit `BookDB`. We need to adapt the `expectConsoleCalls` to use `runTestApp`.

```haskell
expectConsoleCalls :: BookDB -> [ConsoleTapeEntry] -> TestApp a -> IO a
expectConsoleCalls db tape' f = do
  tape <- newIORef tape'

  res <- runTestApp Env {db = db, tape = tape} f

  rest <- readIORef tape
  unless
    (null rest)
    (fail $ "Expected more calls to happen " <> show rest)

  pure res
```

The rest of the helper functions for test don't differ much from the previous alternative. `popTape` needs to get the `IORef` via `asks`. The `HasConsole TestApp` is the same as before.

```haskell
popTape :: String -> TestApp ConsoleTapeEntry
popTape msg = do
  tape' <- asks tape
  current <- liftIO $ readIORef tape'
  when
    (null current)
    (fail $ "Unexpected call " <> msg)
  liftIO $ writeIORef tape' (tail current)
  pure (head current)

instance HasConsole TestApp where
  getStringInput arg = do
    entry <- popTape $ "GetStringInput " <> show arg
    case entry of
      GetStringInput arg' res' | arg' == arg -> pure res'
      _ -> fail $ "Expected call matching " <> show entry <> " got GetStringInput " <> show arg

  printLine arg = do
    entry <- popTape $ "PrintLine " <> show arg
    case entry of
      PrintLine arg' res' | arg' == arg -> pure res'
      _ -> fail $ "Expected call matching " <> show entry <> " got PrintLine " <> show arg
```

The `HasReadOnlyBookDB Test` instance is the same as in the production code.

```haskell
instance HasReadOnlyBookDB TestApp where
  findBook :: String -> TestApp [Book]
  findBook q = do
    db' <- asks db
    liftIO $ Books.findBook db' q
```

`ReaderT` design pattern is a more precise usage of type classes. Having implicit context via `Env` and working with `IO`/`MonadIO` offers better ergonomics than the handler pattern with `Task`. But the handler pattern is built on simpler concepts. 

> [!note] You can find a working copy of this code in `app5` and `app5-test` in [github:bcardiff/lambda-library](https://github.com/bcardiff/lambda-library)