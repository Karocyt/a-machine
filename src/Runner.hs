module Runner where


import System.Exit (exitFailure)

import Machine

-- should return the tape in its final stage
-- can return errorz as a string to without using Either ?

runMachine :: String -> Machine -> String
runMachine t m = "Machine running..."