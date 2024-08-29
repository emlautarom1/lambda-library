module Support where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Main (ConsoleHandler (..))

mockConsoleHandler :: [String] -> IO (IORef [String], ConsoleHandler)
mockConsoleHandler inputs = do
  inputsTape <- newIORef inputs
  printLineTape <- newIORef []
  pure
    ( printLineTape,
      ConsoleHandler
        { getStringInput = \_ -> do
            current <- readIORef inputsTape
            writeIORef inputsTape (tail current)
            pure (head current),
          printLine = \line -> do
            modifyIORef printLineTape (\l -> l ++ [line])
        }
    )

data ConsoleHandlerTape = GetStringInput String String | PrintLine String ()
  deriving (Eq, Show)

expectConsoleCalls :: [ConsoleHandlerTape] -> (ConsoleHandler -> IO a) -> IO a
expectConsoleCalls tape' f = do
  tape <- newIORef tape'

  let popTape msg = do
        current <- readIORef tape
        when
          (null current)
          (fail $ "Unexpected call " <> msg)
        writeIORef tape (tail current)
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

  res <- f mock

  rest <- readIORef tape
  unless
    (null rest)
    (fail $ "Expected more calls to happen " <> show rest)

  pure res
