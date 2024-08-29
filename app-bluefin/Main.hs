module Main where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Books hiding (findBook)
import Books qualified as Book
import Control.Monad (forM_)
import System.IO

-- To mimic ConsoleHandler we use [Dynamic effects](https://hackage.haskell.org/package/bluefin-0.0.6.1/docs/Bluefin-Compound.html#g:6)

data Console e = Console
  { getStringInputImpl :: String -> Eff e String,
    printLineImpl :: String -> Eff e ()
  }

-- The actual functions to use can't be used as records
-- Defining HasField manually lead to which I didn't dig enough to fix it.
--
--
-- instance (e :> es) => HasField "getStringInput" (Console e) (String -> Eff es String) where
--   getField = getStringInput
--
--
-- app-bluefin/Main.hs:31:10: error:
--     • Illegal instance declaration for
--         ‘HasField "getStringInput" (Console e) (String -> Eff es String)’
--         The coverage condition fails in class ‘HasField’
--           for functional dependency: ‘x r -> a’
--         Reason: lhs types ‘"getStringInput"’, ‘Console e’
--           do not jointly determine rhs type ‘String -> Eff es String’
--         Un-determined variable: es
--     • In the instance declaration for
--         ‘HasField "getStringInput" (Console e) (String -> Eff es String)’

getStringInput :: (e :> es) => Console e -> String -> Eff es String
getStringInput e = useImpl . getStringInputImpl e

printLine :: (e :> es) => Console e -> String -> Eff es ()
printLine e = useImpl . printLineImpl e

runConsole :: forall e1 es r. (e1 :> es) => IOE e1 -> (forall e. Console e -> Eff (e :& es) r) -> Eff es r
runConsole io k =
  useImplIn
    k
    Console
      { getStringInputImpl = \prompt -> effIO io $ do
          putStr prompt
          hFlush stdout
          getLine,
        printLineImpl = effIO io . putStrLn
      }

newtype ReadOnlyBookDB e = ReadOnlyBookDB
  { findBookImpl :: String -> Eff e [Book]
  }

findBook :: (e :> es) => ReadOnlyBookDB e -> String -> Eff es [Book]
findBook e = useImpl . findBookImpl e

runReadOnlyBookDB :: forall e1 es r. (e1 :> es) => BookDB -> IOE e1 -> (forall e. ReadOnlyBookDB e -> Eff (e :& es) r) -> Eff es r
runReadOnlyBookDB db io k =
  useImplIn
    k
    ReadOnlyBookDB
      { findBookImpl = \query -> effIO io (Book.findBook db query)
      }

main :: IO ()
main =
  withDB
    "./books.db"
    ( \db ->
        runEff $ \io ->
          runConsole io $ \c ->
            runReadOnlyBookDB db io $ \rodb ->
              main' c rodb
    )

main' :: (e1 :> es, e2 :> es) => Console e1 -> ReadOnlyBookDB e2 -> Eff es ()
main' c db = do
  printLine c "Welcome to the Library"
  loop c db

loop :: (e1 :> es, e2 :> es) => Console e1 -> ReadOnlyBookDB e2 -> Eff es ()
loop c db = do
  query <- getStringInput c "Search: "
  case query of
    "" ->
      printLine c "Bye!"
    _ -> do
      books <- findBook db query
      if null books
        then
          printLine c $ "No books found for: " <> query
        else
          printBookList c books
      loop c db

printBookList :: (e1 :> es) => Console e1 -> [Book] -> Eff es ()
printBookList c books =
  forM_ books (\book -> printLine c $ " * " <> book.title <> ", " <> book.author)
