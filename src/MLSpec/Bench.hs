{-# LANGUAGE OverloadedStrings #-}

module MLSpec.Bench where

import Control.Exception
import Criterion.Main
import Data.Char
import Data.List
import Paths_mlspec_bench
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

-- Register benchmarks

benchMain = do cmd   <- getEnv "BENCHMARK_COMMAND"
               input <- getContents
               defaultMain [
                   bgroup "command" [mkBench cmd input]
                 ]

mkBench :: String -> String -> Benchmark
mkBench cmd stdin = Criterion.Main.env (inputs cmd stdin) go
  where go stdio = bench ("Running " ++ show cmd)
                         (nfIO (run cmd stdio))

-- Functions to benchmark

run :: String -> Input -> IO String
run cmd (stdin, sout, serr) = do
  stdout <- openFile sout AppendMode
  stderr <- openFile serr AppendMode
  (c, o, e) <- readCreateProcessWithExitCode (mkCmd cmd) stdin
  hPutStr stdout ("\n-----\n" ++ o)
  hPutStr stderr ("\n-----\n" ++ e)
  hClose stdout
  hClose stderr
  case c of
    ExitSuccess   -> return ()
    ExitFailure i -> error ("'" ++ cmd ++ "' exited with code " ++ show i)
  return o

-- Test data

type Deferred a = () -> a
type Input = (String, FilePath, FilePath)

inputs :: String -> String -> IO Input
inputs cmd stdin = do (stdout, stderr) <- outputPaths cmd
                      return (stdin, stdout, stderr)

outputPaths cmd = do Just d <- lookupEnv "BENCH_DIR"
                     let out = open d "stdout"
                         err = open d "stderr"
                     return (out, err)
  where open d x = d ++ "/outputs/" ++ hash cmd ++ "." ++ x

mkCmd c = proc c []

-- Helpers

hash = map (\c -> if isAscii c && isAlphaNum c then c else '_')

err :: (Show a) => a -> IO ()
err = hPutStrLn stderr . show
