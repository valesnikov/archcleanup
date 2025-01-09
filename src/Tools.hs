module Tools where

import Data.HashMap.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphs (Graph (Graph), Pack (..), IdTable)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createPipe, runProcess, waitForProcess)
import qualified Data.IntSet as IntSet

printByLines :: (Show a) => [a] -> IO ()
printByLines = foldr ((>>) . print) (pure ())

-- | calls the program with the name and arguments
-- and writes it stdout in response
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

checkExecutable :: Text -> IO Bool
checkExecutable exec = isJust <$> findExecutable (Text.unpack exec)

genGraphviz :: Graph -> IdTable Text -> String
genGraphviz (Graph hmap) t = "strict digraph {\n" ++ inner ++ "}\n"
  where
    inner = foldr (\x l -> l ++ perPack x) "" (Map.elems hmap)
    perPack pack = foldr (\x l -> l ++ genLine name x) style deps
      where
        name = Text.unpack $ snd t $ pId pack
        deps = map (Text.unpack . snd t) (IntSet.elems $ pDepends pack)
        style = "    \"" ++ name ++ "\" [fontsize=" ++ fsize ++ "]\n"
        fsize = show $ 14 + 4 * IntSet.size (pDepends pack)
    genLine n1 n2 = "    \"" ++ n1 ++ "\" -> \"" ++ n2 ++ "\"\n"