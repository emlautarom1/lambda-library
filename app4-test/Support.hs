{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Support where

import Books qualified
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Main
import Result

data ConsoleTapeEntry = GetStringInput String String | PrintLine String ()
  deriving (Eq, Show)

newtype TestAppIO a = TestAppIO (ReaderT (IORef [ConsoleTapeEntry]) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

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

instance CanReadBookDB TestAppIO where
  findBook db q = liftIO $ Books.findBook db q

expectConsoleCallsIO :: [ConsoleTapeEntry] -> TestAppIO a -> IO a
expectConsoleCallsIO tape' (TestAppIO f) = do
  tape <- newIORef tape'

  res <- runReaderT f tape

  rest <- readIORef tape
  unless
    (null rest)
    (fail $ "Expected more calls to happen " <> show rest)

  pure res

newtype TestApp a = TestApp (StateT (String -> [Books.Book], [ConsoleTapeEntry]) Result a)
  deriving (Functor, Applicative, Monad, MonadFail)

-- Using StateT without IO means we don't have MonadFail
-- We need to build our own Result monad to have it back.

popTape :: String -> TestApp ConsoleTapeEntry
popTape msg = do
  (f, current) <- TestApp get
  when
    (null current)
    (fail $ "Unexpected call " <> msg)
  TestApp $ put (f, tail current)
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

instance CanReadBookDB TestApp where
  findBook _ q = do
    (f, _) <- TestApp get
    pure $ f q

expectConsoleCalls :: (String -> [Books.Book]) -> [ConsoleTapeEntry] -> TestApp a -> IO a
expectConsoleCalls find tape (TestApp f) = do
  -- Maybe we could have make expectConsoleCalls return a TestApp a to avoid using IO entirely
  -- but that would require an instance of Test.Hspec.Example
  -- which, when implementing, will require using IO eitherway
  --
  -- evaluateExample :: e -> Params -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result

  case runStateT f (find, tape) of
    Ok (v, (_, rest)) -> do
      unless
        (null rest)
        (fail $ "Expected more calls to happen " <> show rest)

      pure v
    Err err -> fail err
