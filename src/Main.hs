{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM)
import Data.HashTable.IO qualified as HashTable
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createPipe, runProcess, waitForProcess)

data Package = Package
  { name :: Text,
    size :: Int64,
    explicit :: Bool,
    provides :: Set Text,
    depends :: Set Text,
    optional :: Set Text,
    optrequir :: Set Text,
    required :: Set Text
  }
  deriving (Eq, Show, Read)

type PackageTable = HashTable.BasicHashTable Text Package

expacName :: Text
expacName = "expac"

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
          provides = Set.fromList $ Text.words provides',
          depends = Set.fromList $ Text.words depends',
          optional = Set.fromList $ Text.words optional',
          optrequir = Set.fromList [],
          required = Set.fromList $ Text.words required'
        }
    f _ = error "Invalid input"

editKey :: (Package -> Maybe Package) -> Text -> PackageTable -> IO ()
editKey f key hmap = HashTable.mutate hmap key $ (,()) . f . fromJust

editKeyIO :: (Package -> IO (Maybe Package)) -> Text -> PackageTable -> IO ()
editKeyIO f key hmap = HashTable.mutateIO hmap key $ fmap (,()) . f . fromJust

editTableIO :: (Package -> IO (Maybe Package)) -> PackageTable -> IO ()
editTableIO f hmap = HashTable.mapM_ (\(k, _) -> editKeyIO f k hmap) hmap

editTable :: (Package -> Maybe Package) -> PackageTable -> IO ()
editTable f hmap = HashTable.mapM_ (\(k, _) -> editKey f k hmap) hmap

cleanOptionals :: PackageTable -> IO ()
cleanOptionals hmap = editTableIO edit hmap
  where
    edit v = do
      newO <- filterM check (Set.toAscList $ optional v)
      pure $ Just v {optional = Set.fromAscList newO}
    check o = isJust <$> HashTable.lookup hmap o

computeOptRequire :: PackageTable -> IO ()
computeOptRequire hmap = editTableIO mopts hmap
  where
    mopts v = mapM_ edit (optional v) >> pure (Just v)
      where
        edit n = editKey addReq n hmap
        addReq k = Just $ k {optrequir = Set.insert (name v) (optrequir k)}

-- only for packages with empty 'required'
deletePackage :: PackageTable -> Text -> IO ()
deletePackage hmap name' = editTable perPack hmap
  where
    perPack v
      | name v == name' = Nothing
      | otherwise =
          Just $
            v
              { required = Set.delete name' (required v),
                optrequir = Set.delete name' (optrequir v),
                optional = Set.delete name' (optional v)
              }

keys :: PackageTable -> IO [Text]
keys = HashTable.foldM (\acc (k, _) -> pure $ k : acc) []

findOrphans :: PackageTable -> IO [Text]
findOrphans hmap = do
  ks <- keys hmap
  filterM check ks
  where
    check :: Text -> IO Bool
    check key = do
      Just v <- HashTable.lookup hmap key
      pure $ null (required v) && not (explicit v)

main :: IO ()
main = do
  Just raw <- getExpacQuery ["%n|%E|%S|%o|%N|%w|%m"] -- name,
  let packs = parseToPackages $ map (Text.splitOn "|") (Text.lines raw)
  hmap <- HashTable.fromList [(name x, x) | x <- packs] :: IO PackageTable
  cleanOptionals hmap
  computeOptRequire hmap
  print =<< findOrphans hmap