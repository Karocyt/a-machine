module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Parser
import Machine
import Runner

-- main :: IO ()
main = do
    putStrLn "-- BEGIN ----------------------------------"
    args <- processArgs =<< getArgs
    putStrLn "Arguments validated"
    content <- readFile $ args !! 0
    putStrLn "Description read"
    let tape = args !! 1
    -- print content

    case (parseMachine content) of
        Left str  -> print str
        Right m   -> putStrLn "Machine loaded" >> runMachine m tape

    putStrLn "-- END ------------------------------------"

