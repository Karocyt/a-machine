module Parser where

import System.Exit (exitFailure)

import Machine
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)

-- processArgs :: [String] -> IO String
processArgs xs  | length xs /= 1    = putStrLn ("Usage: ./a-machine [filename]\n\twhere 'filename' is a json encoded valid machine description.") >> exitFailure
                | otherwise         = pure $ xs !! 0 -- :: IO String

-- parseMachine :: String -> Either String Machine
parseMachine c = eitherDecode (pack c) :: Either String Machine

-- lexMachine :: Machine -> Bool
lexMachine m = True