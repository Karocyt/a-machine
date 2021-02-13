module Parser where

import System.Exit (exitFailure)

import Machine
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)

-- input should have only alphabet chars and no blank caracter, so Machine needed to validate it

-- processArgs :: [String] -> IO String
processArgs xs  | length xs /= 2    = putStrLn ("Usage: ./a-machine desc.json tape\nwhere:\n\t- 'desc.json' is a json encoded file containing a valid machine description\n\t- 'tape' is a string of instructions from the machine alphabet") >> exitFailure
                | otherwise         = pure xs -- :: [IO String]

-- parseMachine :: String -> Either String Machine
parseMachine c = eitherDecode (pack c) :: Either String Machine

-- checkTape useless as we can error on execution if invalid char

-- lexMachine :: Either String Machine -> Either String Machine
-- lexMachine (Left str) = Left str
-- lexMachine (Right m) = Right m