{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Criterion.Main
import qualified Data.ByteString as B
import Data.List
import Paths_mlspec_bench
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import qualified System.Process.ByteString as P

-- Register benchmarks

main = defaultMain [
    bgroup "MLSpec" (map mlspecBench clusters)
  ]

mlspecBench :: Int -> Benchmark
mlspecBench n = Criterion.Main.env (inputs n) go
  where go stdin = bench (show n ++ " cluster(s)")
                         (nfIO (run n stdin))

-- Functions to benchmark

run :: Int -> String -> IO String
run n stdin = do cmd' <- cmd
                 (c, o, e) <- readCreateProcessWithExitCode cmd' stdin
                 err (if null e
                         then "No stderr from MLSpec"
                         else "MLSpec stderr: '" ++ e ++ "'")
                 err ("MLSpec output: " ++ o)
                 case c of
                   ExitSuccess   -> return ()
                   ExitFailure i -> error ("MLSpec exited with code " ++ show i)
                 return o
  where cmd = return $ proc "nix-shell" ["-p", "mlspec", "--run", "MLSpec"]

-- Test data

clusters = [1, 2, 11]  -- See data-files in mlspec-bench.cabal

clusterFile :: Int -> IO FilePath
clusterFile n = getDataFileName ("data/list-extras.formatted." ++ show n)

inputs :: Int -> IO String
inputs n = clusterFile n >>= readFile

-- Helpers

err :: (Show a) => a -> IO ()
err = hPutStrLn stderr . show
