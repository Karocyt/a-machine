module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Parser
import Machine
import Runner

-- main :: IO ()
main = do
    args <- processArgs =<< getArgs
    content <- readFile $ args !! 0
    let tape = args !! 1
    -- print content
    case (parseMachine content) of
        Left str  -> print str
        Right m   -> runMachine m tape
    print "Ok"

