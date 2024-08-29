{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Books hiding (findBook)
import Books qualified as Book
import Control.Monad
import Effectful
import Effectful.Dispatch.Dynamic
import System.IO

data Console :: Effect where
  GetStringInput :: String -> Console m String
  PrintLine :: String -> Console m ()

type instance DispatchOf Console = Dynamic

-- The following getStringInput and printLine can be generated via makeEffect
-- makeEffect ''Console

getStringInput :: (HasCallStack, Console :> es) => String -> Eff es String
getStringInput prompt = send (GetStringInput prompt)

printLine :: (HasCallStack, Console :> es) => String -> Eff es ()
printLine line = send (PrintLine line)

runConsoleIO :: (IOE :> es) => Eff (Console : es) a -> Eff es a
runConsoleIO = interpret $ \_ -> \case
  GetStringInput prompt -> liftIO $ do
    putStr prompt
    hFlush stdout
    getLine
  PrintLine line -> liftIO $ putStrLn line

data ReadOnlyBookDB :: Effect where
  FindBook :: String -> ReadOnlyBookDB m [Book]

type instance DispatchOf ReadOnlyBookDB = Dynamic

findBook :: (HasCallStack, ReadOnlyBookDB :> es) => String -> Eff es [Book]
findBook query = send (FindBook query)

runReadOnlyBookDB :: (IOE :> es) => BookDB -> Eff (ReadOnlyBookDB : es) a -> Eff es a
runReadOnlyBookDB db = interpret $ \_ -> \case
  FindBook q -> liftIO $ Book.findBook db q

main :: IO ()
main = do
  withDB
    "./books.db"
    ( \db -> do
        runEff
          . runConsoleIO
          . runReadOnlyBookDB db
          $ main'
    )

main' :: (Console :> es, ReadOnlyBookDB :> es) => Eff es ()
main' = do
  printLine "Welcome to the Library"
  loop

loop :: (Console :> es, ReadOnlyBookDB :> es) => Eff es ()
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

printBookList :: (Console :> es) => [Book] -> Eff es ()
printBookList books =
  forM_ books (\book -> printLine $ " * " <> book.title <> ", " <> book.author)
