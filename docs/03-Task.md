# A Haskell tour on side-effects: Part 3, Task

In this alternative we are going to continue with the handler pattern, but instead of using `IO a` as the return type in the functions of the handlers we are going to use a new type `Task a`.

The goal is to restrict other side-effects from happening in `main'`, `loop`, and `printBookList`. When the return type is `IO` it means that we can do any side-effects.

Why do we care? Restricting other side-effects from happening by the type of the function is helpful to understand what the function can do, how can we refactor it, how things can go wrong. But requires some additional efforts.

When including third party libraries having these benefits are even more important. For sure there is always the chance that an `unsafePerformIO` is sneaked somewhere, but let's assume we all understand why that function should be very rarely used.

So, when the return type is `IO` we can do any kind of side-effects. Like deleting a file.

```haskell
main' :: ConsoleHandler -> BookDB -> IO ()
main' c db = do
  c.printLine "Welcome to the Library"
  removeFile "/etc/passwd"
  loop c db
```

The key thing for `main'`, `loop`, and `printBookList` functions to work is that they return a `Monad` so `do` notation can be used. We can define a new monad named `Task`, and adapt the `ConsoleHandler` to return such type.

```haskell
data ConsoleHandler = ConsoleHandler
  { getStringInput :: String -> Task String,
    printLine :: String -> Task ()
  }
```

Under the hood a `Task` needs to perform some `IO`. But as long as there are no functions that returns a `Task` directly like `removeFile :: Task ()` we are going to be able to restrict side-effects. How? Becauser we will always need a handler to get a `Task`.

This also means that our `findBook :: BookDB -> String -> IO [Book]` is not going to work direclty. Yet that function could be provided by a package that we are in no control. We need a handler to interact in a controlled way with the DB. Bonus we can make that handle to perform read-only operations in the DB. 

```haskell
newtype ReadOnlyBookDBHandler = ReadOnlyBookDBHandler
  { findBook :: String -> Task [Book]
  }
```

A `ReadOnlyBookDBHandler` value will point the database to use. There is no need to have`BookDB` argument when using this handler.

Before diving into how this handlers are created let's see them in action. The functions are now a bit more verbose in the arguments. And `printBookList` receives only the needed `ConsoleHandler` to perform its action.

```haskell
main' :: ConsoleHandler -> ReadOnlyBookDBHandler -> Task ()
main' c db = do
  c.printLine "Welcome to the Library"
  loop c db

loop :: ConsoleHandler -> ReadOnlyBookDBHandler -> Task ()
loop c db = do
  query <- c.getStringInput "Search: "
  case query of
    "" ->
      c.printLine "Bye!"
    _ -> do
      books <- db.findBook query
      if null books
        then
          c.printLine $ "No books found for: " <> query
        else
          printBookList c books
      loop c db

printBookList :: ConsoleHandler -> [Book] -> Task ()
printBookList c books =
  forM_ books (\book -> c.printLine $ " * " <> book.title <> ", " <> book.author)
```

Creating the handlers itself is usually done when the application start, in a `main :: IO ()`.

```haskell
consoleHandler :: IO ConsoleHandler

readOnlyBookDBHandler :: BookDB -> IO ReadOnlyBookDBHandler

main :: IO ()
main = do
  withDB
    "./books.db"
    ( \db -> do
        c <- consoleHandler
        db' <- readOnlyBookDBHandler db
        Task.perform (main' c db')
    )
```

The `Task.perform :: Task a -> IO a` function will actually execute the `Task`. 

Again, since all the `Task` values that perform side-effects are created from handlers we are certain that `main'` will no perform unwanted side-effects. But `main` is able to use all the power of `IO` directly.

Now, how we define `Task`? It's going to be an opaque type that implements `Monad`.

```haskell
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Task ( Task, perform )

newtype Task a = Task {run :: IO a}
  deriving (Functor, Applicative, Monad, MonadFail)
  
perform :: Task a -> IO a
perform = run
```

Thanks to `GeneralisedNewtypeDeriving` we can derive `Monad` and `MonadFail` from the wrapped `IO`, but otherwise we can manually define it.

With that we can already write `Task` using `pure`, `fail`, `>>=` and `do` notation.

We still have pending to create the actual handlers. For that we need to create a `Task` from an `IO` because `Book.findBook` and `putStrLn` are `IO`. But we don't want to be able to do `\ s -> Task { run = putStrLn s }`, that would mean not restricting the side-effects at all.

The `Task` module export also the following definitions.

```haskell
data DoAnythingHandler

doAnythingHandler :: IO DoAnythingHandler

doAnything :: DoAnythingHandler -> IO a -> Task a
```

A `DoAnythingHandler` value can _only_ be obtained from the `doAnythingHandler` function which returns an `IO`. So, we can't call it from a `Task`, only from an `IO`.

Then we can convert an `IO` to a `Task` using `doAnything` that requires a `DoAnythingHandler` value.

This means that converting an `IO` to a `Task` can only happen inside an `IO`. 

Funny thing, the `DoAnythingHandler` value is not actually used. It acts as a type level lock.

```haskell
data DoAnythingHandler = DoAnythingHandler

doAnythingHandler :: IO DoAnythingHandler
doAnythingHandler = pure DoAnythingHandler

doAnything :: DoAnythingHandler -> IO a -> Task a
doAnything _ io = Task {run = io}
```

We can now implement the `consoleHandler` and `readOnlyBookDBHandler` functions.

```haskell
consoleHandler :: IO ConsoleHandler
consoleHandler = do
  h <- doAnythingHandler
  pure
    ConsoleHandler
      { getStringInput = \prompt -> doAnything h $ do
          putStr prompt
          hFlush stdout
          getLine,
        printLine = doAnything h . putStrLn
      }

readOnlyBookDBHandler :: BookDB -> IO ReadOnlyBookDBHandler
readOnlyBookDBHandler db = do
  h <- doAnythingHandler
  pure
    ReadOnlyBookDBHandler
      { findBook = doAnything h . Book.findBook db
      }      
```

Let's take a look at the test in this alternative. The handlers allow us to change the `ConsoleHandler` but also the handler for the database.

Both handlers need to return `Task`, not `IO` as it was in the previous alternative. This change requires some boilerplate to wrap the `IORef`'s `IO` functions as `Task`. The `Task` is something we introduced and nothing in the Haskell ecosystem will be ready for this right away.

The `expectConsoleCalls` is almost identicall as before. Just a `IO` and `Task` dance that needs to happen.

```haskell
data ConsoleTapeEntry = GetStringInput String String | PrintLine String ()
  deriving (Eq, Show)

expectConsoleCalls :: [ConsoleTapeEntry] -> (ConsoleHandler -> Task a) -> IO a
expectConsoleCalls tape' f = do
  h <- doAnythingHandler

  tape <- newIORef tape'

  let popTape msg = do
        current <- doAnything h $ readIORef tape
        when
          (null current)
          (fail $ "Unexpected call " <> msg)
        doAnything h $ writeIORef tape (tail current)
        pure (head current)

  let mock =
        ConsoleHandler
          { getStringInput = \arg -> do
              entry <- popTape $ "GetStringInput " <> show arg
              case entry of
                GetStringInput arg' res' | arg' == arg -> pure res'
                _ -> fail $ "Expected call matching " <> show entry <> " got GetStringInput " <> show arg,
            printLine = \arg -> do
              entry <- popTape $ "PrintLine " <> show arg
              case entry of
                PrintLine arg' res' | arg' == arg -> pure res'
                _ -> fail $ "Expected call matching " <> show entry <> " got PrintLine " <> show arg
          }

  res <- Task.perform (f mock)

  rest <- readIORef tape
  unless
    (null rest)
    (fail $ "Expected more calls to happen " <> show rest)

  pure res
```

Regarding the `ReadOnlyBookDBHandler` we could implement one for testing, but we can also choose to use the real one. Accessing the database directly.

```haskell
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
```

This approach is used in [nri-prelude](https://hackage.haskell.org/package/nri-prelude) where there is a richer `Task` type. There are bunch of additional packages like [nri-redis](https://hackage.haskell.org/package/nri-redis), [nri-http](https://hackage.haskell.org/package/nri-http), and [nri-postgresql](https://hackage.haskell.org/package/nri-postgresql) that wraps some packages from the ecosystem to work with the handler pattern and `Task`.

> [!note] You can find a working copy of this code in `app3` and `app3-test` in [github:bcardiff/lambda-library](https://github.com/bcardiff/lambda-library)