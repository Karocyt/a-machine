module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Parser
import Machine
import Runner

-- both args and content might need to be IO (Either String a) ?
-- I broke my teeth on readFile, my best being IO (Either error Bytestring) at some point 

-- main :: IO ()
main = do
    putStrLn "-- BEGIN ----------------------------------"
    args <- (getArgs >>= processArgs) -- parenthesis for aesthetics
    putStrLn "Arguments checked"
    let tape = args !! 1
    content <- readFile $ head args
    putStrLn "Description read"
    -- print content

    case (parseMachine content) of
        Left str  -> print str
        Right m   -> putStrLn $ runMachine tape m

    putStrLn "-- END ------------------------------------"

