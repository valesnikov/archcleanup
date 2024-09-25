{-# LANGUAGE OverloadedStrings #-}

module Main where

import Algs qualified
import Data.HashMap qualified as Map
import Data.Maybe (fromJust)
import Graphs
import Pacman qualified

main :: IO ()
main = do
  graph <- fromJust <$> Pacman.getGraph
  let af = Algs.verifyDigraph graph
  case af of
    Left ps -> print ps
    Right dg -> do
      let Graph hmap = Algs.computeDeps dg
      print $ Map.lookup "pacman" hmap