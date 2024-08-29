module Support where

import Data.ByteString.UTF8 qualified as BSU
import Test.Main

runWithStdinAndCapture :: [String] -> IO () -> IO [String]
runWithStdinAndCapture input f = do
  result <- captureProcessResult (withStdin (BSU.fromString (unlines input)) f)
  pure $ lines (BSU.toString result.prStdout)
