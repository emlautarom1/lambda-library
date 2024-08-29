module Support where

import Control.Monad (unless, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Main (ConsoleHandler (..))
import Task

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
