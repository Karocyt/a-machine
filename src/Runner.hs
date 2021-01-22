module Runner where


import System.Exit (exitFailure)

import Machine

runMachine :: Machine -> String -> IO ()
runMachine m t = putStrLn "Machine running..."