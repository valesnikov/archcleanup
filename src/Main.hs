{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM)
import Data.HashTable.IO qualified as HashTable
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createPipe, runProcess, waitForProcess)

data Package = Package
  { name :: Text,
    size :: Int64,
    explicit :: Bool,
    provides :: [Text],
    depends :: [Text],
    optional :: [Text],
    required :: [Text]
  }
  deriving (Eq, Show, Read)

type PackageTable = HashTable.BasicHashTable Text Package

expacName :: Text
expacName = "expac"

printByLines :: (Show a) => [a] -> IO ()
printByLines = foldr ((>>) . print) (return ())

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
    ExitSuccess -> return $ Just outStr
    _ -> return Nothing

getExpacQuery :: [Text] -> IO (Maybe Text)
getExpacQuery args = getCmdOutput expacName ("-Q" : args)

parseToPackages :: [[Text]] -> [Package]
parseToPackages = map f
  where
    f [name', depends', provides', optional', required', explicit', size'] =
      Package
        { name = name',
          size = read (Text.unpack size'),
          explicit = explicit' == "explicit",
          provides = Text.words provides',
          depends = Text.words depends',
          optional = Text.words optional',
          required = Text.words required'
        }
    f _ = error "Invalid input"

editTable :: PackageTable -> (Package -> IO (Maybe Package)) -> IO ()
editTable hmap f = HashTable.mapM_ edit hmap
  where
    edit (key, _) = HashTable.mutateIO hmap key $ fmap (,()) . f . fromJust

cleanOptionals :: PackageTable -> IO ()
cleanOptionals hmap = editTable hmap edit
  where
    edit v = filterM check (optional v) >>= (\o -> return $ Just v {optional = o})
    check o = isJust <$> HashTable.lookup hmap o

main :: IO ()
main = do
  Just raw <- getExpacQuery ["%n|%E|%S|%o|%N|%w|%m"] -- name,
  let packs = parseToPackages $ map (Text.splitOn "|") (Text.lines raw)
  hmap <- HashTable.fromList [(name x, x) | x <- packs] :: IO PackageTable
  cleanOptionals hmap
  res <- HashTable.lookup hmap "pacman"
  print res
