module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Parser
import Machine
import Runner

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

