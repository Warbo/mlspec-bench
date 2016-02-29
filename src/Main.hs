{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Criterion.Main
import Data.List
import Paths_mlspec_bench
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

-- Register benchmarks

main = defaultMain [
    bgroup "MLSpec" (map mlspecBench clusters)
  ]

mlspecBench :: Int -> Benchmark
mlspecBench n = Criterion.Main.env (inputs n) go
  where go stdin = bench (show n ++ " cluster(s)")
                         (nfIO (run n stdin))

-- Functions to benchmark

run :: Int -> Input -> IO String
run n (stdin, sout, serr) = do
  stdout <- openFile sout AppendMode
  stderr <- openFile serr AppendMode
  (c, o, e) <- readCreateProcessWithExitCode cmd stdin
  hPutStr stdout ("\n-----\n" ++ o)
  hPutStr stderr ("\n-----\n" ++ e)
  hClose stdout
  hClose stderr
  case c of
    ExitSuccess   -> return ()
    ExitFailure i -> error ("explore-theories exited with code " ++ show i)
  return o
  where cmd = proc "explore-theories" []

-- Test data

clusters = [1, 2] --, 11]  -- See data-files in mlspec-bench.cabal

clusterFile :: Int -> IO FilePath
clusterFile n = getDataFileName ("data/list-extras.formatted." ++ show n)

type Deferred a = () -> a
type Input = (String, FilePath, FilePath)

inputs :: Int -> IO Input
inputs n = do stdin            <- clusterFile n >>= readFile
              (stdout, stderr) <- outputPaths n
              return (stdin, stdout, stderr)

outputPaths n = do Just d <- lookupEnv "BENCH_DIR"
                   let out = open d "stdout"
                       err = open d "stderr"
                   return (out, err)
  where open d x = d ++ "/outputs/" ++ show n ++ "." ++ x

-- Helpers

err :: (Show a) => a -> IO ()
err = hPutStrLn stderr . show
