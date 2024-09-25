module Tools where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createPipe, runProcess, waitForProcess)

printByLines :: (Show a) => [a] -> IO ()
printByLines = foldr ((>>) . print) (pure ())

getCmdOutput :: Text -> [Text] -> IO (Maybe Text)
getCmdOutput cmdName args = do
  (readEnd, writeEnd) <- createPipe
  handle <-
    runProcess
      (Text.unpack cmdName)
      (map Text.unpack args)
      Nothing
      Nothing
      Nothing
      (Just writeEnd)
      Nothing
  outStr <- Text.hGetContents readEnd
  code <- waitForProcess handle
  case code of
    ExitSuccess -> pure $ Just outStr
    _ -> pure Nothing