module Main (main) where

import Distribution.Simple ( defaultMainWithHooks, simpleUserHooks
                           , UserHooks(runTests))
import System.Cmd (system)

main :: IO ()
main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

-- Runs the testsuite
runTests' _ _ _ _ = system cmd >> return ()
  where testdir = "tests"
        testcmd = "runhaskell ./Main.hs"
        cmd = "cd " ++ testdir ++ " && " ++ testcmd
