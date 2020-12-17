module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Parser
import Machine

-- main :: IO ()
main = do
    content <- readFile =<< processArgs =<< getArgs
    -- print content
    print $ parseMachine content
    print "Ok"

