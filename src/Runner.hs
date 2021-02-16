module Runner where


import System.Exit (exitFailure)

import Machine

-- NEEDS:
-- - check if nextTransition in finals
--   - Right State
-- - Execute/check if transition exists
--   - Left "error blabla"
-- - Move pos
--   - Left "Stay on the dancefloor"
-- - Tail recursion
-- State might get bigger depending on how we handle bonuses (keeping history, counting iterations...)

runMachine :: Machine -> State -> Either String State
runMachine machine state = Left $ "It's ALIIIIIVE:\n" ++ (show machine) ++ "\n" ++ (show state)