{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Support where

import Books
import Control.Monad.Reader
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Main

data ConsoleTapeEntry = GetStringInput String String | PrintLine String ()
  deriving (Eq, Show)

data Env = Env {db :: BookDB, tape :: IORef [ConsoleTapeEntry]}

newtype TestApp a = TestApp (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadFail)

runTestApp :: Env -> TestApp a -> IO a
runTestApp env (TestApp f) =
  runReaderT f env

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

instance HasReadOnlyBookDB TestApp where
  findBook q = do
    db' <- asks db
    liftIO $ Books.findBook db' q

expectConsoleCalls :: BookDB -> [ConsoleTapeEntry] -> TestApp a -> IO a
expectConsoleCalls db tape' f = do
  tape <- newIORef tape'

  res <- runTestApp Env {db = db, tape = tape} f

  rest <- readIORef tape
  unless
    (null rest)
    (fail $ "Expected more calls to happen " <> show rest)

  pure res
